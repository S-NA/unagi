/* -*-mode:c;coding:utf-8; c-basic-offset:2;fill-column:70;c-file-style:"gnu"-*-
 *
 * Copyright (C) 2013 Arnaud "arnau" Fontaine <arnau@mini-dweeb.org>
 *
 * D-Bus libev integration based on Awesome Window Manager project.
 *
 * This  program is  free  software: you  can  redistribute it  and/or
 * modify  it under the  terms of  the GNU  General Public  License as
 * published by the Free Software  Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
 * General Public License for more details.
 *
 * You should have  received a copy of the  GNU General Public License
 *  along      with      this      program.      If      not,      see
 *  <http://www.gnu.org/licenses/>.
 */

/** \file
 *  \brief Communication with D-Bus
 */

#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

#include "structs.h"
#include "dbus.h"

#define _INTERFACE_ADD_MATCH_FMT "type='method_call',interface='%s'"

/** Request D-Bus Bus name and Add Match for Interface as well
 *  (required so that D-Bus Bus knows that it should be sent it
 *  here). This is the caller responsability to flush the messages by
 *  calling dbus_connection_flush.
 *
 * \param name D-Bus Bus and Interface names
 * \return true if successful
 */
bool
unagi_dbus_request_name(const char *name)
{
  DBusError err;
  dbus_error_init(&err);

  int ret = dbus_bus_request_name(globalconf.dbus_connection,
                                  name,
                                  // One Unagi instance per D-Bus session
                                  DBUS_NAME_FLAG_DO_NOT_QUEUE,
                                  &err);

  if(dbus_error_is_set(&err))
    {
      unagi_warn("%s: Failed to request Name: %s", name, err.message);
      dbus_error_free(&err);
      return false;
    }

  if(ret != DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER &&
     ret != DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER)
    {
      unagi_warn("%s: Failed to become Primary Owner", name);
      return false;
    }

  char interface_name[strlen(_INTERFACE_ADD_MATCH_FMT) + strlen(name) + 1];
  sprintf(interface_name, _INTERFACE_ADD_MATCH_FMT, name);
  dbus_bus_add_match(globalconf.dbus_connection, interface_name, &err);
  if(dbus_error_is_set(&err))
    {
      unagi_warn("%s: Failed to register Interface: %s", name, err.message);
      dbus_error_free(&err);

      dbus_bus_release_name(globalconf.dbus_connection, name, &err);
      if(dbus_error_is_set(&err))
        dbus_error_free(&err);

      return false;
    }

  return true;
}

/** libev callback to process queued D-Bus Messages, processed here
 *  for core Interface Messages (org.minidweeb.unagi) or dispatched
 *  to plugins Interface (org.minidweeb.unagi.plugin.NAME).
 *
 *  Currently, only 'exit' Message is implemented for the core, but it
 *  may be extended in the future.
 *
 * \todo Handle Introspectable and Disconnected Messages
 * \todo Implement restart of Unagi through D-Bus?
 */
static void
_dbus_process_messages(EV_P_ ev_io *w __attribute__((unused)),
                       int revents __attribute__((unused)))
{
  dbus_connection_read_write(globalconf.dbus_connection, 0);

  bool do_exit = false;
  unsigned int msg_sent_counter = 0;
  DBusMessage *msg = NULL;
  while((msg = dbus_connection_pop_message(globalconf.dbus_connection)))
    {
      bool msg_processed = false;
      const int msg_type = dbus_message_get_type(msg);
      const char *msg_interface = dbus_message_get_interface(msg);
      const char *msg_member = dbus_message_get_member(msg);
      const char *error_name = NULL;
      if(msg_interface != NULL &&
         strcmp(msg_interface, UNAGI_DBUS_NAME) == 0)
        {
          if(msg_type == DBUS_MESSAGE_TYPE_METHOD_CALL &&
             strcmp(msg_member, "exit") == 0)
            {
              /* Delay exit after flushing the connection for the
                 success reply and handling all other messages (to
                 avoid memory leaks) */
              do_exit = true;
              msg_processed = true;
            }
        }
      else if(msg_interface != NULL &&
              strcmp(msg_interface, UNAGI_DBUS_NAME_PLUGIN_PREFIX) > 0)
        {
          unagi_plugin_t *plugin;
          for(plugin = globalconf.plugins; plugin; plugin = plugin->next)
            if(plugin->enable &&
               plugin->vtable->dbus_process_message &&
               strcmp(msg_interface + strlen(UNAGI_DBUS_NAME_PLUGIN_PREFIX),
                      plugin->vtable->name) == 0)
              {
                error_name = (*plugin->vtable->dbus_process_message)(msg);
                msg_processed = true;
                break;
              }
        }
      /* Probably D-Bus-specific messages (such as
         org.freedesktop.DBus.NameAcquired when a Bus name has been
         requested)... Ignore them for now */
      else
        {
          unagi_debug("Message not processed: type=%d, interface=%s, member=%s",
                      msg_type, msg_interface, msg_member);

          error_name = DBUS_ERROR_NOT_SUPPORTED;
          msg_processed = true;
        }

      /* Send an error (if the message expects a reply) for any
         unknown member of core or plugins D-Bus Interface */
      if(!msg_processed)
        {
          unagi_warn("Message not processed: type=%d, interface=%s, member=%s",
                     msg_type, msg_interface, msg_member);

          error_name = DBUS_ERROR_UNKNOWN_METHOD;
        }

      unagi_dbus_send_reply_from_processed_message(msg,
                                                   error_name == NULL,
                                                   error_name);
      dbus_message_unref(msg);
      msg_sent_counter++;
    }

  if(msg_sent_counter)
    dbus_connection_flush(globalconf.dbus_connection);

  if(do_exit)
    // Let atexit() handle cleanup
    exit(0);
}

/** Connect to D-Bus and request Bus/Interface names for the core
 *  only. For plugins, this is done in check_requirements for
 *  flexibility sake (most of the time in check_requirements() hook as
 *  to enter Expose plugin for example).
 *
 *  Initializing libev to process D-Bus messages is done separately as
 *  libev main loop is not initialized at this point.
 *
 * \see unagi_dbus_ev_init
 * \see main
 *
 * \return true if successful, otherwise disable D-Bus support
 */
bool
unagi_dbus_init(void)
{
  DBusError err;
  dbus_error_init(&err);

  globalconf.dbus_connection = dbus_bus_get(DBUS_BUS_SESSION, &err);
  if(dbus_error_is_set(&err))
    {
      unagi_warn("Cannot connect to D-Bus session bus: %s", err.message);
      dbus_error_free(&err);
      goto dbus_init_failed;
    }

  dbus_connection_set_exit_on_disconnect(globalconf.dbus_connection, false);

  if(!unagi_dbus_request_name(UNAGI_DBUS_NAME))
    goto dbus_init_failed;

  dbus_connection_flush(globalconf.dbus_connection);

  return true;

 dbus_init_failed:
  unagi_dbus_cleanup();
  return false;
}

/** Add libev watcher on D-Bus FD to process messages, through the
 *  main loop
 *
 *  \return true if successful, otherwise disable D-Bus
 */
bool
unagi_dbus_ev_init(void)
{
  int fd;
  if(!dbus_connection_get_unix_fd(globalconf.dbus_connection, &fd))
    {
      unagi_warn("Cannot get D-Bus Connection FD");
      unagi_dbus_cleanup();
      return false;
    }

  fcntl(fd, F_SETFD, FD_CLOEXEC);
  ev_io_init(&globalconf.dbus_event_io, _dbus_process_messages, fd, EV_READ);
  ev_io_start(EV_DEFAULT_UC_ &globalconf.dbus_event_io);
  ev_unref(EV_DEFAULT_UC);

  return true;
}

/** Helper sending success/error D-Bus reply. As the whole program
 *  prints messages on stdout/stderr and especially since D-Bus
 *  support is not mandatory, send only a generic error message.
 *
 * \param msg Message to be replied to
 * \param is_success true if the Messages was processed successfully
 * \param error_name Specific error name, otherwise generic DBUS_ERROR_FAILED
 */
void
unagi_dbus_send_reply_from_processed_message(DBusMessage *msg,
                                             bool is_success,
                                             const char *error_name)
{
  if(dbus_message_get_no_reply(msg))
    return;

  DBusMessage *reply;
  if(is_success)
    {
      reply = dbus_message_new_method_return(msg);

      DBusMessageIter iter;
      dbus_message_iter_init_append(reply, &iter);

      uint32_t return_value = true;
      dbus_message_iter_append_basic(&iter, DBUS_TYPE_BOOLEAN, &return_value);
    }
  else
    {
      if(error_name == NULL)
        error_name = DBUS_ERROR_FAILED;

      char *error_message = NULL;
      if(strcmp(error_name, DBUS_ERROR_FAILED) == 0)
        error_message = (char *) "Check Unagi messages for further information...";

      reply = dbus_message_new_error(msg, error_name, error_message);
    }

  if(!dbus_connection_send(globalconf.dbus_connection, reply, NULL))
    unagi_warn("Failed to send message reply (interface=%s, member=%s)",
               dbus_message_get_interface(msg),
               dbus_message_get_member(msg));

  dbus_message_unref(reply);
}

/** Release previously requested D-Bus Bus and Interface names
 *
 * \see unagi_dbus_request_name
 * \param name D-Bus Bus and Interface names
 */
void
unagi_dbus_release_name(const char *name)
{
  DBusError err;
  dbus_error_init(&err);

  char *interface_name[strlen(_INTERFACE_ADD_MATCH_FMT) + strlen(name) + 1];
  sprintf((char *) interface_name, _INTERFACE_ADD_MATCH_FMT, name);
  dbus_bus_add_match(globalconf.dbus_connection, (char *) interface_name, &err);
  if(dbus_error_is_set(&err))
    dbus_error_free(&err);

  dbus_bus_release_name(globalconf.dbus_connection, name, &err);
  if(dbus_error_is_set(&err))
    dbus_error_free(&err);
}

/** Cleanup memory allocated by D-Bus and also stop libev watcher on
 *  D-Bus FD before unreferencing connection (and not closing as it is
 *  shared in libdbus). This function is called in case of
 *  unrecoverable D-Bus error or on program exit.
 */
void
unagi_dbus_cleanup(void)
{
  if(globalconf.dbus_connection == NULL)
    return;

  unagi_dbus_release_name(UNAGI_DBUS_NAME);

  if(globalconf.dbus_event_io.fd >= 0)
    {
      ev_ref(EV_DEFAULT_UC);
      ev_io_stop(EV_DEFAULT_UC_ &globalconf.dbus_event_io);
      globalconf.dbus_event_io.fd = -1;
    }

  dbus_connection_unref(globalconf.dbus_connection);
  globalconf.dbus_connection = NULL;

  // Clear internal D-Bus caches
  dbus_shutdown();
}
