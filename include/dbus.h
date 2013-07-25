/* -*-mode:c;coding:utf-8; c-basic-offset:2;fill-column:70;c-file-style:"gnu"-*-
 *
 * Copyright (C) 2013 Arnaud "arnau" Fontaine <arnau@mini-dweeb.org>
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

#ifndef UNAGI_DBUS_H
#define UNAGI_DBUS_H

#define UNAGI_DBUS_NAME "org.minidweeb.unagi"
#define UNAGI_DBUS_NAME_PLUGIN_PREFIX UNAGI_DBUS_NAME ".plugin."

bool unagi_dbus_request_name(const char *);
bool unagi_dbus_init(void);
bool unagi_dbus_ev_init(void);
void unagi_dbus_send_reply_from_processed_message(DBusMessage *, bool, const char *);
void unagi_dbus_release_name(const char *);
void unagi_dbus_cleanup(void);

#endif
