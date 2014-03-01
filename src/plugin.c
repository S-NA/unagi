/* -*-mode:c;coding:utf-8; c-basic-offset:2;fill-column:70;c-file-style:"gnu"-*-
 *
 * Copyright (C) 2009 Arnaud "arnau" Fontaine <arnau@mini-dweeb.org>
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
 *  \brief Effects plugins management
 */

#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>

#include "plugin.h"
#include "structs.h"
#include "util.h"
#include "plugin_common.h"

/** Load the plugin with the given name
 *
 * \param name The plugin name
 * \return The plugin loaded or NULL if any error
 */
static unagi_plugin_t *
_unagi_plugin_load(const char *name)
{
  unagi_plugin_t *new_plugin = calloc(1, sizeof(unagi_plugin_t));
  char *error;

  /* Clear any existing error */
  dlerror();

  /* Open the plugin in the  plugins directory given as a command line
     parameter or the default path set during compilation */
  new_plugin->dlhandle = unagi_plugin_common_dlopen(globalconf.plugins_dir, name);
  if((error = dlerror()))
    goto plugin_load_error;

  /* Load the virtual table of  the plugins containing the pointers to
     the plugins functions */
  new_plugin->vtable = dlsym(new_plugin->dlhandle, "plugin_vtable");
  if((error = dlerror()))
    goto plugin_load_error;

  unagi_debug("Plugin %s loaded", name);
  return new_plugin;	  

 plugin_load_error:
  unagi_debug("Can't load plugin %s", name);
  unagi_fatal_no_exit("%s", error);
  free(new_plugin);
  return NULL;
}

static inline void
_unagi_plugin_append_global(unagi_plugin_t *previous_plugin,
                            unagi_plugin_t *new_plugin)
{
  if(!previous_plugin)
    globalconf.plugins = new_plugin;
  else
    {
      previous_plugin->next = new_plugin;
      new_plugin->prev = previous_plugin;
    }
}

/** Load all the plugins given in the configuration file */
void
unagi_plugin_load_all(void)
{
  const unsigned int plugins_nb = cfg_size(globalconf.cfg, "plugins");
  if(!plugins_nb)
    return;

  unagi_plugin_t *opacity_plugin = NULL;
  unagi_plugin_t *plugin = NULL;
  for(unsigned int plugin_n = 0; plugin_n < plugins_nb; plugin_n++)
    {
      unagi_plugin_t *new_plugin = _unagi_plugin_load(cfg_getnstr(globalconf.cfg,
                                                                  "plugins",
                                                                  plugin_n));
      if(!new_plugin)
        ;
      else if(strcmp(new_plugin->vtable->name, "opacity") == 0)
        opacity_plugin = new_plugin;
      else
        {
          _unagi_plugin_append_global(plugin, new_plugin);
          plugin = new_plugin;
        }
    }

  if(opacity_plugin)
    _unagi_plugin_append_global(plugin, opacity_plugin);
}

/** Enable the plugin if it meets the requirements */
void
unagi_plugin_check_requirements(void)
{
  for(unagi_plugin_t *plugin = globalconf.plugins; plugin; plugin = plugin->next)
    plugin->enable = (!plugin->vtable->check_requirements ? true :
		     (*plugin->vtable->check_requirements)());
}

/** Look for a plugin from its name
 *
 * \param name The plugin name
 * \return Return the plugin or NULL
 */
unagi_plugin_t *
unagi_plugin_search_by_name(const char *name)
{
  for(unagi_plugin_t *plugin = globalconf.plugins; plugin; plugin = plugin->next)
    if(strcmp(plugin->vtable->name, name) == 0)
      return plugin;

  return NULL;
}

/** Unload all the plugins and their allocated memory */
void
unagi_plugin_unload_all(void)
{
  unagi_plugin_t *plugin = globalconf.plugins;
  unagi_plugin_t *plugin_next;

  while(plugin != NULL)
    {
      plugin_next = plugin->next;
      dlclose(plugin->dlhandle);
      free(plugin);
      plugin = plugin_next;
    }
}
