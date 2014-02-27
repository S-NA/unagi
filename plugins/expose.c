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
 *  \brief Exposé effect plugin
 *
 *  This plugin implements (roughly) Expose  feature as seen in Mac OS
 *  X and Compiz  (known as Scale plugin) but is  not really optimised
 *  (yet) because it repaints all the  windows even if the content has
 *  not been changed.  The window slots  could be arranged in a better
 *  way by including the window geometry in the computation.
 *
 *  It relies  on _NET_CLIENT_LIST  (required otherwise the  plugin is
 *  disabled),  _NET_ACTIVE_WINDOW  atoms   (required  and  stored  in
 *  '_expose_global.atoms'    structure)   and    _NET_CURRENT_DESKTOP
 *  (required when a  Window on another desktop is  selected to switch
 *  to it  and activate  the window) to  get respectively  the clients
 *  managed  by the  window manager  and the  current focused  window.
 *  These atoms values are updated in a lazy way (e.g.  by sending the
 *  GetProperty requests on initialisation and PropertyNotify and then
 *  getting the reply as late as needed).
 *
 *  The rendering is performed in  the following steps when the plugin
 *  is enabled ('_expose_enter'):
 *
 *   1/  Create the  slots where  each window  will be  put  by simply
 *      dividing the screen in  strips according the current number of
 *      windows ('_expose_create_slots').
 *
 *   2/ Assign each  window to a slot based  on the Euclidian distance
 *      between their center ('_expose_assign_windows_to_slots').
 *
 *   3/ Map all windows which were unmapped to get their content using
 *      NameWindowPixmap  Composite   request  (when  the   window  is
 *      unmapped, the  content is not guaranteed to  be preserved) and
 *      also set OverrideRedirect attribute  to ensure that the window
 *      manager will not care about them anymore.
 *
 *   4/ For  each window, create  a new 'unagi_window_t'  object, thus
 *      creating  a new  list of  windows which  will override  global
 *      Windows list  and itree  while the Expose  is running.  If the
 *      window needs  to be rescaled  (e.g.  when the window  does not
 *      fit the slot), then it is done through Render.
 */

#include <math.h>
#include <string.h>
#include <stdlib.h>

#include <xcb/xcb.h>
#include <xcb/xcb_ewmh.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_aux.h>
#include <xkbcommon/xkbcommon.h>

#include "structs.h"
#include "window.h"
#include "plugin.h"
#include "atoms.h"
#include "util.h"
#include "key.h"
#include "event.h"
#include "dbus.h"

#define _PLUGIN_NAME "expose"
#define _PLUGIN_CONFIG_FILENAME "plugin_" _PLUGIN_NAME ".conf"
#define _DBUS_NAME UNAGI_DBUS_NAME_PLUGIN_PREFIX _PLUGIN_NAME

/** Expose window */
typedef struct
{
  /** Rescaled window */
  unagi_window_t *window;
  /** If the window was unmapped before enabling the plugin */
  bool was_unmapped;
} _expose_scale_unagi_window_t;

/** Each window is contained within a slot */
typedef struct
{
  /** Slot geometry */
  xcb_rectangle_t extents;
  /** Nearest window associated with this slot */
  unagi_window_t *window;
  /** Rescaled window */
  _expose_scale_unagi_window_t scale_window;
} _expose_window_slot_t;

/** Atoms required for this plugin */
typedef struct
{
  /** _NET_CLIENT_LIST atom cookie */
  xcb_get_property_cookie_t client_list_cookie;
  /** _NET_CLIENT_LIST atom value */
  xcb_ewmh_get_windows_reply_t *client_list;
  /** _NET_ACTIVE_WINDOW atom cookie */
  xcb_get_property_cookie_t active_window_cookie;
  /** _NET_ACTIVE_WINDOW atom value */
  xcb_window_t *active_window;
  /** _NET_CURRENT_DESKTOP atom cookie */
  xcb_get_property_cookie_t current_desktop_cookie;
  /** _NET_CURRENT_DESKTOP atom value */
  uint32_t *current_desktop;
} _expose_atoms_t;

typedef struct
{
  uint32_t nwindows;
  uint8_t nstrips;
  unsigned int nwindows_per_strip;
  xcb_randr_get_crtc_info_reply_t *crtc;
  _expose_window_slot_t *slots;
} _expose_crtc_window_slots_t;

/** Global variables of this plugin */
static struct
{
  /** libconfuse configuration */
  cfg_t *cfg;
  /** Entered Expose? */
  bool entered;
  /** Atoms structure */
  _expose_atoms_t atoms;
  /** Opacity of Windows */
  struct {
    uint16_t focus;
    uint16_t unfocus;
  } window_opacity;
  /** Allow to specify a command format to run to select a Window
      rather (%d is replaced by the actual Window ID) than
      _NET_CURRENT_DESKTOP and then _NET_ACTIVE_WINDOW ClientMessage
      (EWMH standard is utterly broken for multiple CRTCs support
      through XRandR) */
  const char *window_select_cmd_fmt;
  /** Slots for thumbnails per CRTC */
  _expose_crtc_window_slots_t *crtc_slots;
  /** Current CRTC and Slot (Window) */
  _expose_crtc_window_slots_t *current_crtc;
  _expose_window_slot_t *current_slot;
  /** Global windows list context before Expose overrides it while running */
  unagi_window_t *windows_head_before_enter;
  unagi_window_t *windows_tail_before_enter;
  unagi_util_itree_t *windows_itree_before_enter;
  /** Mouse pointer position */
  int16_t pointer_x;
  int16_t pointer_y;
  /** Navigation KeySyms */
  struct {
    xcb_keysym_t crtc_cycle;
    xcb_keysym_t window_up;
    xcb_keysym_t window_prev;
    xcb_keysym_t window_next;
    xcb_keysym_t window_down;
    xcb_keysym_t window_select;
    xcb_keysym_t quit;
  } keys;
} _expose_global;

extern unagi_plugin_vtable_t plugin_vtable;

#define _WINDOW_GET_OPACITY(opacity)                            \
  ((opacity) == 1.0 ? UINT16_MAX :                              \
   ((uint16_t) (((double) (opacity) * 0xffffffff) / 0xffff)))

#define _CONFIGURATION_VALIDATE_OPACITY(pname)                          \
  static int                                                            \
  _expose_configuration_validate_##pname##_opacity(cfg_t *cfg,          \
                                                   cfg_opt_t *opt)      \
  {                                                                     \
    double opacity = cfg_opt_getnfloat(opt, 0);                         \
    if(opacity <= 0.0 || opacity > 1.0)                                 \
      {                                                                 \
        cfg_error(_expose_global.cfg,                                   \
                  "Option '%s': Opacity must be > 0.0 and <= 1.0",      \
                  opt->name);                                           \
                                                                        \
        return -1;                                                      \
      }                                                                 \
                                                                        \
    _expose_global.window_opacity.pname = _WINDOW_GET_OPACITY(opacity); \
    return 0;                                                           \
  }

_CONFIGURATION_VALIDATE_OPACITY(focus)
_CONFIGURATION_VALIDATE_OPACITY(unfocus)

#undef _CONFIGURATION_VALIDATE_OPACITY

#define _VALIDATE_KEY_FUNC(keyname)                                     \
  static int                                                            \
  _expose_configuration_validate_##keyname##_key(cfg_t *cfg,            \
                                                 cfg_opt_t *opt)        \
  {                                                                     \
    const char *n = cfg_opt_getnstr(opt, 0);                            \
    _expose_global.keys.keyname = xkb_keysym_from_name(n,               \
                                                       XKB_KEYSYM_NO_FLAGS); \
    if(_expose_global.keys.keyname == XKB_KEY_NoSymbol)                 \
      {                                                                 \
        cfg_error(_expose_global.cfg,                                   \
                  "Option '%s' does not specify a valid key",           \
                  opt->name, cfg->name);                                \
                                                                        \
        return -1;                                                      \
      }                                                                 \
                                                                        \
    return 0;                                                           \
  }

_VALIDATE_KEY_FUNC(crtc_cycle)
_VALIDATE_KEY_FUNC(window_up)
_VALIDATE_KEY_FUNC(window_prev)
_VALIDATE_KEY_FUNC(window_next)
_VALIDATE_KEY_FUNC(window_down)
_VALIDATE_KEY_FUNC(window_select)
_VALIDATE_KEY_FUNC(quit)

#undef _CONFIGURATION_VALIDATE_KEY

static void
_expose_parse_configuration(void)
{
  cfg_opt_t windows_keys_opts[] = {
    CFG_STR_LIST("up", "{Up}", CFGF_NONE),
    CFG_STR_LIST("previous", "{Left}", CFGF_NONE),
    CFG_STR_LIST("next", "{Right}", CFGF_NONE),
    CFG_STR_LIST("down", "{Down}", CFGF_NONE),
    CFG_STR_LIST("select", "{Return}", CFGF_NONE),
    CFG_END()
  };

  cfg_opt_t keys_opts[] = {
    CFG_STR_LIST("crtc-cycle", "{Tab}", CFGF_NONE),
    CFG_SEC("windows", windows_keys_opts, CFGF_NONE),
    CFG_STR_LIST("quit", "{Escape}", CFGF_NONE),
    CFG_END()
  };

  cfg_opt_t opts[] = {
    CFG_FLOAT("focus-window-opacity", 1.0, CFGF_NONE),
    CFG_FLOAT("unfocus-window-opacity", 0.75, CFGF_NONE),
    CFG_STR("window-select-cmd", NULL, CFGF_NONE),
    CFG_SEC("keys", keys_opts, CFGF_NONE),
    CFG_END()
  };

  _expose_global.cfg = cfg_init(opts, CFGF_NONE);

#define _VALIDATE_SET_FUNC(name, vf_suffix)                             \
  cfg_set_validate_func(_expose_global.cfg, name,                       \
                        _expose_configuration_validate_##vf_suffix);

  _VALIDATE_SET_FUNC("focus-window-opacity", focus_opacity)
  _VALIDATE_SET_FUNC("unfocus-window-opacity", unfocus_opacity)
  _VALIDATE_SET_FUNC("keys|crtc-cycle", crtc_cycle_key)
  _VALIDATE_SET_FUNC("keys|windows|up", window_up_key)
  _VALIDATE_SET_FUNC("keys|windows|previous", window_prev_key)
  _VALIDATE_SET_FUNC("keys|windows|next", window_next_key)
  _VALIDATE_SET_FUNC("keys|windows|down", window_down_key)
  _VALIDATE_SET_FUNC("keys|windows|select", window_select_key)
  _VALIDATE_SET_FUNC("keys|quit", quit_key)
#undef _VALIDATE_SET_FUNC

  char *fname_path =
    unagi_util_get_configuration_filename_path(_PLUGIN_CONFIG_FILENAME);

  if(cfg_parse(_expose_global.cfg, fname_path) != CFG_SUCCESS)
    {
      free(fname_path);
      unagi_fatal("Can't parse configuration file");
    }

  free(fname_path);

  _expose_global.window_select_cmd_fmt = cfg_getstr(_expose_global.cfg,
                                                    "window-select-cmd");

  if(!strlen(_expose_global.window_select_cmd_fmt))
    _expose_global.window_select_cmd_fmt = NULL;
}

static inline void
_expose_pointer_move_center(const unagi_window_t *window)
{
  xcb_warp_pointer(globalconf.connection,
                   XCB_NONE,
                   globalconf.screen->root,
                   0, 0, 0, 0,
                   window->geometry->x + window->geometry->width / 2,
                   window->geometry->y + window->geometry->height / 2);
}

static inline bool
_expose_coordinates_within_slot(_expose_window_slot_t *slot,
                                int16_t x,
                                int16_t y)
{
  return (slot &&
          x >= slot->extents.x &&
          x <= slot->extents.x + slot->extents.width &&
          y >= slot->extents.y &&
          y <= slot->extents.y + slot->extents.height);
}

static bool
_expose_update_current_crtc_and_slot(int16_t x, int16_t y)
{
  for(_expose_crtc_window_slots_t *crtc = _expose_global.crtc_slots;
      crtc - _expose_global.crtc_slots < globalconf.crtc_len;
      crtc++)
    {
      if(!crtc->nwindows)
        continue;

      for(_expose_window_slot_t *slot = crtc->slots;
          slot - crtc->slots < crtc->nwindows;
          slot++)
        if(_expose_coordinates_within_slot(slot, x, y))
          {
            _expose_global.current_crtc = crtc;
            _expose_global.current_slot = slot;
            return true;
          }
    }

  return false;
}

/** Called  on  dlopen() to  initialise  memory  areas and  also  send
 *  GetProperty  requests on  the  root  window for  _NET_CLIENT_LIST,
 *  _NET_ACTIVE_WINDOW   and  _NET_CURRENT_DESKTOP   atoms  to   avoid
 *  blocking when these values will be needed
 */
static void __attribute__((constructor))
expose_constructor(void)
{
  memset(&_expose_global, 0, sizeof(_expose_global));

  _expose_global.atoms.client_list = NULL;
  _expose_global.atoms.active_window = NULL;
  _expose_global.atoms.current_desktop = NULL;

  /* Send  the requests  to check  whether the  atoms are  present and
     whose replies will  be got when actually calling  the function to
     check atoms which are required */
  _expose_global.atoms.client_list_cookie =
    xcb_ewmh_get_client_list_unchecked(&globalconf.ewmh,
                                       globalconf.screen_nbr);

  _expose_global.atoms.active_window_cookie =
    xcb_ewmh_get_active_window_unchecked(&globalconf.ewmh,
                                         globalconf.screen_nbr);

  _expose_global.atoms.current_desktop_cookie =
    xcb_ewmh_get_current_desktop_unchecked(&globalconf.ewmh,
                                           globalconf.screen_nbr);

  _expose_parse_configuration();
}

/** Update    the     values    of     _NET_CLIENT_LIST    (required),
 *  _NET_ACTIVE_WINDOW (required)  and _NET_CURRENT_DESKTOP (required)
 *  if the GetProperty  has been sent but not  already retrieved (thus
 *  on  plugin  initialisation  or  on  PropertyNotify  event).
 *
 * \param atoms Atoms information
 */
static void
_expose_update_atoms_values(_expose_atoms_t *atoms)
{
#define CHECK_REQUIRED_ATOM(kind, kind_type, atom_name)			\
  if(atoms->kind##_cookie.sequence)					\
    {									\
      if(!atoms->kind)							\
	atoms->kind = calloc(1, sizeof(kind_type));			\
									\
      if(!xcb_ewmh_get_##kind##_reply(&globalconf.ewmh,                 \
				      atoms->kind##_cookie,		\
				      atoms->kind,			\
				      NULL))				\
	{								\
	  unagi_warn("Plugin cannot be enabled: Cannot get %s (check with "   \
               "'xprop -root')", #atom_name);                           \
	  unagi_util_free(&atoms->kind);					\
	}								\
									\
      /* Reset the cookie sequence for the next request */		\
      atoms->kind##_cookie.sequence = 0;				\
    }

  CHECK_REQUIRED_ATOM(client_list, xcb_ewmh_get_windows_reply_t, _NET_CLIENT_LIST)
  CHECK_REQUIRED_ATOM(active_window, xcb_window_t, _NET_ACTIVE_WINDOW)
  CHECK_REQUIRED_ATOM(current_desktop, uint32_t, _NET_CURRENT_DESKTOP)
}

/** Check whether the plugin can actually be enabled. It only requires
 *  D-Bus to enter Expose. After that, this is done through keyboard
 *  shortcuts (added after entering) or mouse.
 *
 *  D-Bus could have be used for keys after entering Expose, but it
 *  means that these specific keys would have to be defined in WM
 *  configuration, would trigger unnecessary calls and especially this
 *  allows to define much more simple keys (such as binding 'Escape'
 *  to quit and 'Left/Right' keys to go to the previous/next windows).
 *
 * \todo make the GrabKey completely asynchronous
 * \return true if the plugin can be enabled
 */
static bool
expose_check_requirements(void)
{
  if(globalconf.dbus_connection == NULL)
    return false;

  /* Request D-Bus name org.minidweeb.unagi.plugin.expose to be able
     to enter Expose. This should never failed, hence the dirty hack
     to reset dbus-related exported variables */
  if(!unagi_dbus_request_name(_DBUS_NAME))
    {
      unagi_warn("D-Bus failed because of the warnings above, therefore "
                 "this plugin will be only useable through the mouse.");

      plugin_vtable.dbus_process_message = NULL;
      return false;
    }

  return true;
}

/** Check whether the window actually needs to be rescaled
 *
 * \param slot_extents The slots rectangle
 * \param window_width The window original width including border
 * \param window_height The window original height including border
 * \return true if the window needs to be rescaled
 */
static inline bool
_expose_window_need_rescaling(xcb_rectangle_t *slot_extents,
			      const uint16_t window_width,
			      const uint16_t window_height)
{
  return slot_extents->width < window_width ||
    slot_extents->height < window_height;
}

static float
_expose_crtc_get_window_visible_ratio(xcb_randr_get_crtc_info_reply_t *crtc_info,
                                      int16_t x, int16_t y,
                                      uint16_t width, uint16_t height)
{
  int32_t visible_max_x;
  if(x + width > crtc_info->x + crtc_info->width)
    visible_max_x = crtc_info->x + crtc_info->width;
  else if(x + width < crtc_info->x)
    return 0.0;
  else
    visible_max_x = x + width;

  int32_t visible_max_y;
  if(y + height > crtc_info->y + crtc_info->height)
    visible_max_y = crtc_info->y + crtc_info->height;
  else if(y + height < crtc_info->y)
    return 0.0;
  else
    visible_max_y = y + height;

  int32_t visible_min_x;
  if(x < crtc_info->x)
    visible_min_x = crtc_info->x;
  else
    visible_min_x = x;

  int32_t visible_min_y;
  if(y < crtc_info->y)
    visible_min_y = crtc_info->y;
  else
    visible_min_y = y;

  int32_t visible_area = (visible_max_x - visible_min_x) *
    (visible_max_y - visible_min_y);

  if(visible_area <= 0)
    return 0.0;

  return (float) visible_area / (float) (width * height);
}

static void
_expose_crtc_assign_window(unagi_window_t *window)
{
  float max_ratio = 0.0, ratio;
  _expose_crtc_window_slots_t *assigned_crtc = NULL;

  for(unsigned int i = 0; i < globalconf.crtc_len; i++)
    {
      ratio = _expose_crtc_get_window_visible_ratio(globalconf.crtc[i],
                                                    window->geometry->x,
                                                    window->geometry->y,
                                                    window->geometry->width,
                                                    window->geometry->height);

      if(ratio > max_ratio)
        {
          max_ratio = ratio;
          assigned_crtc = _expose_global.crtc_slots + i;
        }
    }

  if(assigned_crtc != NULL)
    {
      assigned_crtc->slots[assigned_crtc->nwindows].window = window;
      assigned_crtc->nwindows++;
    }
}

/** Create the slots where the  window will be arranged. The screen is
 *  divided in  strips of the same  size whose number is  given by the
 *  square root of the number of windows
 *
 * \param nwindows The number of windows
 * \param nwindows_per_strip The number of windows per strip
 * \return The newly allocated slots
 */
static void
_expose_create_slots(_expose_crtc_window_slots_t *crtc_slots)
{
  /* The  screen is  divided  in  strips depending  on  the number  of
     windows */
  crtc_slots->nstrips = (uint8_t) sqrt(crtc_slots->nwindows + 1);

  const uint16_t strip_height = (uint16_t)
    (crtc_slots->crtc->height / crtc_slots->nstrips);

  /* The number of windows per strip depends */
  crtc_slots->nwindows_per_strip = (unsigned int)
    ceilf((float) crtc_slots->nwindows / (float) crtc_slots->nstrips);

  int16_t current_y = crtc_slots->crtc->y;
  int16_t current_x;

  /* Each slot is a rectangle  whose coordinates depends on the number
     of strips and the number of windows */	
  unsigned int slot_n = 0;

  /* Create the strips of windows */
  for(uint8_t strip_n = 0; strip_n < crtc_slots->nstrips; strip_n++)
    {
      current_x = crtc_slots->crtc->x;

      /* Number of slots for this strip which depends on the number of
	 remaining slots (the last strip may contain less windows) */
      const unsigned int strip_slots_n =
        (crtc_slots->nwindows - slot_n > crtc_slots->nwindows_per_strip ?
         crtc_slots->nwindows_per_strip : crtc_slots->nwindows - slot_n);

      const uint16_t slot_width = (uint16_t)
        (crtc_slots->crtc->width / strip_slots_n);

      /* Now create the slots associated to this strip */
      for(unsigned int strip_slot = 0; strip_slot < strip_slots_n; strip_slot++)
	{
	  crtc_slots->slots[slot_n].extents.x = current_x;
	  crtc_slots->slots[slot_n].extents.y = current_y;
	  crtc_slots->slots[slot_n].extents.width = slot_width;
	  crtc_slots->slots[slot_n].extents.height = strip_height;

	  current_x = (int16_t) (current_x + slot_width);
	  ++slot_n;
	}

      current_y = (int16_t) (current_y + strip_height);
    }
}

/** Assign each  window into the  nearest slot based on  the Euclidian
 *  distance between the center of the slot and the window
 *
 * \param nwindows The number of windows
 * \param nwindows_per_strip The number of windows per strip
 * \param slots The slots where the window will be assign
 */
static void
_expose_assign_windows_to_slots(_expose_crtc_window_slots_t *crtc_slots)
{
  _expose_create_slots(crtc_slots);
  _expose_window_slot_t *slots = crtc_slots->slots;

  struct
  {
    unagi_window_t *window;
    /* Coordinates of the window center */
    int16_t x, y;
  } windows[crtc_slots->nwindows];

  /* Prepare the  windows and their information  before assigning them
     to a slot */
  for(uint32_t i = 0; i < crtc_slots->nwindows; i++)
    {
      unagi_window_t *w = slots[i].window;
      windows[i].window = w;
      windows[i].x = (int16_t) (w->geometry->x + w->geometry->width / 2);
      windows[i].y = (int16_t) (w->geometry->y + w->geometry->height / 2);
    }

  /* Assign the windows to its slot using Euclidian distance */
  for(uint32_t slot_n = 0; slot_n < crtc_slots->nwindows; slot_n++)
    {
      const int16_t slot_x = (int16_t) (slots[slot_n].extents.x +
					slots[slot_n].extents.width / 2);

      const int16_t slot_y = (int16_t) (slots[slot_n].extents.y +
					slots[slot_n].extents.height / 2);

      int16_t x, y;
      uint16_t distance, nearest_distance = UINT16_MAX;
      uint32_t window_n_nearest = 0;

      for(uint32_t window_n = 0; window_n < crtc_slots->nwindows; window_n++)
	{
	  if(!windows[window_n].window)
	    continue;

	  x = (int16_t) (windows[window_n].x - slot_x);
	  y = (int16_t) (windows[window_n].y - slot_y);

	  distance = (uint16_t) sqrt(x * x + y * y);

	  if(distance < nearest_distance)
	    {
	      slots[slot_n].window = windows[window_n].window;
	      window_n_nearest = window_n;
	      nearest_distance = distance;
	    }
	}

      windows[window_n_nearest].window = NULL;
    }

  /** Adjust slot width according to the window size
   * \todo Should also handle the window resize to optimize slot width
   */
  for(uint32_t slot_n = 0;
      slot_n < crtc_slots->nwindows;
      slot_n += crtc_slots->nwindows_per_strip)
    {
      /* Number of spare pixels */
      unsigned int slot_spare_pixels = 0;
      /* Number of slots to extend */
      unsigned int slots_to_extend_n = 0;

      for(uint32_t window_strip_n = 0;
          window_strip_n < crtc_slots->nwindows_per_strip;
	  window_strip_n++)
	{
	  /* Set the slot width to the window one if the window is smaller */
	  if(window_width_with_border(slots[window_strip_n].window->geometry) <
	     slots[window_strip_n].extents.width)
	    {
	      slot_spare_pixels += (unsigned int)
		(slots[window_strip_n].extents.width -
		 window_width_with_border(slots[window_strip_n].window->geometry));

	      slots[window_strip_n].extents.width = window_width_with_border(slots[window_strip_n].window->geometry);
	      slots[window_strip_n].extents.x = (int16_t) (slots[window_strip_n].extents.x + (int16_t) slot_spare_pixels);
	    }
	  /* Don't do anything if the window is of the same size */
	  else if(window_width_with_border(slots[window_strip_n].window->geometry) ==
		  slots[window_strip_n].extents.width)
	    continue;
	  /* Number of slots which are going to be extended */
	  else
	    slots_to_extend_n++;
	}

      /* If there is no slot to extend, don't do anything */
      if(slots_to_extend_n <= 1)
	continue;

      uint16_t spare_pixels_per_slot = (uint16_t) (slot_spare_pixels / slots_to_extend_n);

      for(uint32_t window_strip_n = 0;
          window_strip_n < crtc_slots->nwindows_per_strip;
	  window_strip_n++)
	if(window_width_with_border(slots[window_strip_n].window->geometry) >
	   slots[window_strip_n].extents.width)
	  slots[window_strip_n].extents.width = (uint16_t) (slots[window_strip_n].extents.width + spare_pixels_per_slot);
    }
}

/** Prepare the rescaled windows which  are going to be painted on the
 *  screen  by creating  the rescale  window  image and  then put  the
 *  pixels in it from the original window
 *
 * \param slots The windows slots
 * \param scale_window_prev The last window of the previous CRTC
 */
static void
_expose_prepare_windows(_expose_crtc_window_slots_t *crtc_slots,
                        unagi_window_t **scale_window_prev)
{
  _expose_window_slot_t *slot;
  for(unsigned int i = 0; i < crtc_slots->nwindows; i++)
    {
      slot = crtc_slots->slots + i;

      const uint16_t window_width = window_width_with_border(slot->window->geometry);
      const uint16_t window_height = window_height_with_border(slot->window->geometry);
      unagi_window_t *scale_window;

      /* If the window does not need to be rescaled, just use existing window */
      if(!_expose_window_need_rescaling(&slot->extents, window_width, window_height))
	{
	  unagi_debug("No need to scale %jx", (uintmax_t) slot->window->id);

          scale_window = malloc(sizeof(unagi_window_t));
          memcpy(scale_window, slot->window, sizeof(unagi_window_t));

          scale_window->geometry = malloc(sizeof(xcb_get_geometry_reply_t));
          memcpy(scale_window->geometry, slot->window->geometry,
                 sizeof(xcb_get_geometry_reply_t));

          scale_window->next = NULL;
	}
      else
        {
          scale_window = calloc(1, sizeof(unagi_window_t));
          scale_window->id = slot->window->id;
          scale_window->damage = slot->window->damage;
          scale_window->attributes = slot->window->attributes;
          scale_window->rendering = slot->window->rendering;
          /* The Pixmap is needed for previously unmapped windows to
             create the Picture for example with Render */
          scale_window->pixmap = slot->window->pixmap;

          /* The scale window coordinates are the slot ones */
          scale_window->geometry = calloc(1, sizeof(xcb_get_geometry_reply_t));

          /* Border width is always equals to 0 as it is scaled anyway */
          scale_window->geometry->border_width = 0;

          /* Compute the ratio from the  largest side (width or height) of
             the window */
          const float ratio =
            min((float) slot->extents.width / (float) window_width,
                (float) slot->extents.height / (float) window_height);

          scale_window->geometry->width = (uint16_t)
            floorf(ratio * (float) window_width);

          scale_window->geometry->height = (uint16_t)
            floorf(ratio * (float) window_height);

          memset(scale_window->transform_matrix, 0, 16);
          scale_window->transform_matrix[0][0] = 1;
          scale_window->transform_matrix[1][1] = 1;
          scale_window->transform_matrix[2][2] = ratio;

          scale_window->transform_status = UNAGI_WINDOW_TRANSFORM_STATUS_REQUIRED;
        }

      scale_window->geometry->x = slot->extents.x +
        (slot->extents.width - scale_window->geometry->width) / 2;

      scale_window->geometry->y = slot->extents.y +
        (slot->extents.height - scale_window->geometry->height) / 2;

      /* Consider the window non-focused, actually handle later by
         expose_pre_paint() */
      scale_window->transform_opacity = _expose_global.window_opacity.unfocus;

      /* Create the region for the scaled window, added to global
         damaged Region upon receiving DamageNotify event */
      scale_window->region = xcb_generate_id(globalconf.connection);

      xcb_rectangle_t area = {
        .x = scale_window->geometry->x,
        .y = scale_window->geometry->y,
        .width = scale_window->geometry->width,
        .height = scale_window->geometry->height };

      xcb_xfixes_create_region(globalconf.connection,
                               scale_window->region, 1, &area);

      /* Link the previous element with the current one */
      scale_window->prev = *scale_window_prev;
      if(*scale_window_prev)
	(*scale_window_prev)->next = scale_window;

      globalconf.windows_itree = util_itree_insert(globalconf.windows_itree,
                                                   scale_window->id,
                                                   scale_window);

      *scale_window_prev = scale_window;
      slot->scale_window.window = scale_window;

#ifdef __DEBUG__
      unagi_debug("slot: x=%jd, y=%jd, width=%ju, height=%ju",
                  (intmax_t) slot->extents.x, (intmax_t) slot->extents.y,
                  (uintmax_t) slot->extents.width, (uintmax_t) slot->extents.height);

      unagi_debug("scale_window: id=%jx, x=%jd, y=%jd, width=%ju, height=%ju, "
                  "region=%jx, original_region=%jx",
                  (uintmax_t) slot->scale_window.window->id,
                  (intmax_t) slot->scale_window.window->geometry->x,
                  (intmax_t) slot->scale_window.window->geometry->y,
                  (uintmax_t) slot->scale_window.window->geometry->width,
                  (uintmax_t) slot->scale_window.window->geometry->height,
                  (uintmax_t) slot->scale_window.window->region,
                  (uintmax_t) slot->window->region);
#endif
    }
}

static void
_expose_free_memory(void)
{
  unagi_util_itree_free(globalconf.windows_itree);

  globalconf.windows_itree = _expose_global.windows_itree_before_enter;
  globalconf.windows = _expose_global.windows_head_before_enter;
  globalconf.windows_tail = _expose_global.windows_tail_before_enter;

  _expose_window_slot_t *slot;
  for(unsigned int crtc_n = 0; crtc_n < globalconf.crtc_len; crtc_n++)
    {
      for(unsigned int window_n = 0;
          window_n < _expose_global.crtc_slots[crtc_n].nwindows;
          window_n++)
        {
          slot = _expose_global.crtc_slots[crtc_n].slots + window_n;

          /* Unmap the window which were previously mapped and also
             restore override redirect */
          if(slot->scale_window.was_unmapped)
            unagi_window_get_invisible_window_pixmap_finalise(slot->window);

          /* Free memory allocated only for Windows *actually* scaled */
          if(slot->scale_window.window->transform_status !=
             UNAGI_WINDOW_TRANSFORM_STATUS_NONE)
            (*globalconf.rendering->free_window)(slot->window);

          unagi_util_free(&(slot->scale_window.window->geometry));
          xcb_xfixes_destroy_region(globalconf.connection,
                                    slot->scale_window.window->region);

          unagi_util_free(&(slot->scale_window.window));
        }

      unagi_util_free(&_expose_global.crtc_slots[crtc_n].slots);
    }

  unagi_util_free(&_expose_global.crtc_slots);
}

/** Disable the  plugin by unmapping  the windows which  were unmapped
 *  before enabling the plugin and then repaint the screen again
 *
 * \param slots The windows slots
 */
static void
_expose_quit(void)
{
  /* Now ungrab both the keyboard, the pointer and the keys */
  xcb_ungrab_pointer(globalconf.connection, XCB_CURRENT_TIME);
  xcb_ungrab_keyboard(globalconf.connection, XCB_CURRENT_TIME);

  _expose_free_memory();
  _expose_global.entered = false;

  /* Force repaint of the screen as the plugin is now disabled */
  globalconf.force_repaint = true;

  unagi_debug("=> Quit");
}

static bool
_expose_grab_finalize(const xcb_grab_pointer_cookie_t *grab_pointer_cookie,
                      const xcb_grab_keyboard_cookie_t *grab_keyboard_cookie)
{
  xcb_grab_pointer_reply_t *grab_pointer_reply =
    xcb_grab_pointer_reply(globalconf.connection, *grab_pointer_cookie, NULL);

  bool grab_pointer_success = true;
  if(!grab_pointer_reply || grab_pointer_reply->status != XCB_GRAB_STATUS_SUCCESS)
    {
      unagi_warn("Cannot grab pointer");
      grab_pointer_success = false;
    }

  free(grab_pointer_reply);

  xcb_grab_keyboard_reply_t *grab_keyboard_reply =
    xcb_grab_keyboard_reply(globalconf.connection, *grab_keyboard_cookie, NULL);

  bool grab_keyboard_success = true;
  if(!grab_keyboard_reply || grab_keyboard_reply->status != XCB_GRAB_STATUS_SUCCESS)
    {
      unagi_warn("Cannot grab keyboard");
      grab_keyboard_success = false;
    }

  free(grab_keyboard_reply);

  if(!grab_pointer_success ||
     !grab_keyboard_success)
    {
      if(grab_pointer_success)
        xcb_ungrab_pointer(globalconf.connection, XCB_CURRENT_TIME);

      if(grab_keyboard_success)
        xcb_ungrab_keyboard(globalconf.connection, XCB_CURRENT_TIME);

      return false;
    }

  return true;
}

/** Enable  the plugin  by  creating  the windows  slots  and map  the
 *  windows which are not already mapped, then fits the windows in the
 *  slots and create their Pixmap, and finally repaint the screen
 *
 * \param nwindows The numbers of windows on the screen
 * \return The newly allocated slots
 */
static bool
_expose_enter(void)
{
  if(_expose_global.entered)
    return true;

  if(!unagi_atoms_is_supported(globalconf.ewmh._NET_CLIENT_LIST) ||
     !unagi_atoms_is_supported(globalconf.ewmh._NET_ACTIVE_WINDOW) ||
     !unagi_atoms_is_supported(globalconf.ewmh._NET_CURRENT_DESKTOP) ||
     !unagi_atoms_is_supported(globalconf.ewmh._NET_WM_DESKTOP))
    {
      unagi_warn("Plugin cannot be enabled: Required atoms _NET_CLIENT_LIST, "
                 "_NET_ACTIVE_WINDOW, _NET_CURRENT_DESKTOP and/or _NET_WM_DESKTOP "
                 "are not in _NET_SUPPORTED (check with 'xprop -root')");

      return false;
    }

  /* Update the  atoms values  now if it  has been changed  in the
     meantime */
  _expose_update_atoms_values(&_expose_global.atoms);
  if(!_expose_global.atoms.client_list ||
     !_expose_global.atoms.active_window ||
     !_expose_global.atoms.current_desktop)
    return false;

  /* Get  the number  of windows  actually managed  by  the window
     manager (as given by _NET_CLIENT_LIST) */
  const uint32_t nwindows = _expose_global.atoms.client_list->windows_len;
  if(!nwindows)
    {
      unagi_warn("Plugin cannot be enabled: No Windows listed in _NET_CLIENT_LIST "
                 "(check with 'xprop -root')");

      return false;
    }

  _expose_global.crtc_slots = calloc(globalconf.crtc_len,
                                     sizeof(_expose_crtc_window_slots_t));

  for(unsigned int i = 0; i < globalconf.crtc_len; i++)
    {
      _expose_global.crtc_slots[i].crtc = globalconf.crtc[i];
      _expose_global.crtc_slots[i].slots = calloc(nwindows,
                                                  sizeof(_expose_window_slot_t));
    }

  for(uint32_t i = 0; i < nwindows; i++)
    _expose_crtc_assign_window(unagi_window_list_get(_expose_global.atoms.client_list->windows[i]));

  xcb_grab_server(globalconf.connection);

  for(unsigned int crtc_n = 0; crtc_n < globalconf.crtc_len; crtc_n++)
    {
      _expose_assign_windows_to_slots(_expose_global.crtc_slots + crtc_n);

      /* Map windows which where  unmapped otherwise the window content is
         not guaranteed to be preserved while the window is unmapped */
      _expose_window_slot_t *slot = NULL;
      for(uint32_t window_n = 0;
          window_n < _expose_global.crtc_slots[crtc_n].nwindows;
          window_n++)
        {
          slot = _expose_global.crtc_slots[crtc_n].slots + window_n;
          if(slot->window->attributes->map_state != XCB_MAP_STATE_VIEWABLE &&
             !slot->scale_window.was_unmapped)
            {
              unagi_window_get_invisible_window_pixmap(slot->window);
              slot->scale_window.was_unmapped = true;
            }
        }
    }

  /* Reset Pointer position (MotionNotify are only received once
     entering Expose) and before GrabPointer to avoid race
     condition */
  _expose_global.pointer_x = -1;
  _expose_global.pointer_y = -1;

  /* Grab the pointer in an active way to avoid EnterNotify event due
   * to the mapping hack */
  const xcb_grab_pointer_cookie_t grab_pointer_cookie =
    xcb_grab_pointer_unchecked(globalconf.connection,
                               false,
                               globalconf.screen->root,
                               XCB_EVENT_MASK_BUTTON_RELEASE |
                               XCB_EVENT_MASK_POINTER_MOTION,
                               XCB_GRAB_MODE_ASYNC,
                               XCB_GRAB_MODE_ASYNC,
                               globalconf.screen->root,
                               XCB_NONE,
                               XCB_CURRENT_TIME);

  /** Process MapNotify event to get the NameWindowPixmap
   *  \todo get only MapNotify? */
  xcb_aux_sync(globalconf.connection);

  unagi_event_handle_poll_loop(unagi_event_handle);

  xcb_ungrab_server(globalconf.connection);

  /* Grab  the keyboard  in an  active way  to avoid  "weird" behavior
     (e.g. being  able to type in  a window which may  be not selected
     due  to  rescaling)  due   to  the  hack  consisting  in  mapping
     previously unmapped windows to get their Pixmap */
  const xcb_grab_keyboard_cookie_t grab_keyboard_cookie =
    xcb_grab_keyboard_unchecked(globalconf.connection, false, globalconf.screen->root,
				XCB_CURRENT_TIME, XCB_GRAB_MODE_ASYNC,
				XCB_GRAB_MODE_ASYNC);

  _expose_global.windows_itree_before_enter = globalconf.windows_itree;
  globalconf.windows_itree = util_itree_new();

  unagi_window_t *prev_window = NULL;
  for(unsigned int i = 0; i < globalconf.crtc_len; i++)
    {
      _expose_crtc_window_slots_t *crtc_slots = _expose_global.crtc_slots + i;
      _expose_prepare_windows(crtc_slots, &prev_window);
    }

  _expose_global.windows_head_before_enter = globalconf.windows;
  globalconf.windows = _expose_global.crtc_slots[0].slots->scale_window.window;

  _expose_global.windows_tail_before_enter = globalconf.windows_tail;
  globalconf.windows_tail = prev_window;

  if(!_expose_grab_finalize(&grab_pointer_cookie, &grab_keyboard_cookie))
    {
      _expose_free_memory();
      unagi_warn("Plugin cannot be enabled: see the messages above");
      return false;
    }

  globalconf.force_repaint = true;

  _expose_global.entered = true;
  unagi_debug("=> Entered");
  return true;
}

/** Show the selected window, either by:
 *
 *   - If window-select-cmd setting has been set, call that command to
 *     select a Window
 *
 *   - _NET_ACTIVE_WINDOW ClientMessage after changing desktop through
 *     _NET_CURRENT_DESKTOP ClientMessage if necessary
 */
static void
_expose_show_selected_window(void)
{
  unagi_window_t *window = _expose_global.current_slot->window;

  /* Quit before sending any request (before that all Windows have
     override-redirect set and previously unmapped Windows were
     mapped) */
  _expose_quit();

  if(_expose_global.window_select_cmd_fmt)
    {
      /* xcb_window_t is always an uint32_t... */
      const size_t window_select_cmd_len = strlen(_expose_global.window_select_cmd_fmt) + 11;
      char *window_select_cmd = malloc(window_select_cmd_len);
      snprintf(window_select_cmd, window_select_cmd_len,
               _expose_global.window_select_cmd_fmt, window->id);

      int ret;
      if((ret = system(window_select_cmd)) != 0)
        unagi_warn("Failed to select Window %jx: system('%s') failed (status=%d)",
                   (uintmax_t) window->id, window_select_cmd, ret);

      free(window_select_cmd);
    }
  else if(window->id != *_expose_global.atoms.active_window)
    {
      uint32_t window_desktop;
      if(!xcb_ewmh_get_wm_desktop_reply(&globalconf.ewmh,
                                        xcb_ewmh_get_wm_desktop(&globalconf.ewmh,
                                                                window->id),
                                        &window_desktop,
                                        NULL))
        unagi_warn("Could not get the current desktop of selected Window");
      else
        {
          if(window_desktop != *_expose_global.atoms.current_desktop)
            xcb_ewmh_request_change_current_desktop(&globalconf.ewmh,
                                                    globalconf.screen_nbr,
                                                    window_desktop,
                                                    XCB_CURRENT_TIME);

          xcb_ewmh_request_change_active_window(&globalconf.ewmh,
                                                globalconf.screen_nbr,
                                                window->id,
                                                XCB_EWMH_CLIENT_SOURCE_TYPE_OTHER,
                                                XCB_CURRENT_TIME,
                                                XCB_NONE);

          unagi_window_map_raised(window);
        }
    }
}

static void
_expose_up_down_update_current_slot(int16_t x, int16_t y, int index)
{
  if(!_expose_update_current_crtc_and_slot(x, y) ||
     _expose_global.current_crtc->nstrips < 2)
    return;

  const unsigned int cur_window_i =
    _expose_global.current_slot - _expose_global.current_crtc->slots;

  const unsigned int new_line_i =
    mod(((int) (cur_window_i / _expose_global.current_crtc->nwindows_per_strip)) + index,
        (int) _expose_global.current_crtc->nstrips);

  for(_expose_window_slot_t *slot = _expose_global.current_crtc->slots +
        new_line_i * _expose_global.current_crtc->nwindows_per_strip;
      slot - _expose_global.current_crtc->slots < _expose_global.current_crtc->nwindows;
      slot++)
    if(slot->extents.x + slot->extents.width >=
       _expose_global.current_slot->extents.x +
       _expose_global.current_slot->extents.width / 2)
      {
        _expose_global.current_slot = slot;
        _expose_pointer_move_center(_expose_global.current_slot->scale_window.window);
        return;
      }
}

static void
_expose_previous_next_update_current_slot(int16_t x, int16_t y, int index)
{
  if(!_expose_update_current_crtc_and_slot(x, y) ||
     _expose_global.current_crtc->nwindows < 2)
    return;

  _expose_global.current_slot = _expose_global.current_crtc->slots +
    mod(_expose_global.current_slot -
        _expose_global.current_crtc->slots + index,
        _expose_global.current_crtc->nwindows);

  _expose_pointer_move_center(_expose_global.current_slot->scale_window.window);
}

/** Handle Damage Notify and called before the core DamageNotify event handler.
 *
 *  @todo For now repaint the whole content of the rescaled Window,
 *        but this could be optimised by only repainting the damaged
 *        area...
 */
static void
expose_event_handle_damage_notify(xcb_damage_notify_event_t *event,
                                  unagi_window_t *window)
{
  /* Hackish: consider the Window not damaged so the core event
     handler will consider it as never painted and it will be
     repainted it completely */
  if(_expose_global.entered && window->damaged_ratio != 1.0)
    window->damaged = false;
}

/** Handle KeyRelease event
 *
 *  @todo: Implement XKB support, until then xmodmap will not be
 *         considered among other things...
 */
static void
expose_event_handle_key_release(xcb_key_release_event_t *event,
                                unagi_window_t *w __attribute__ ((unused)))
{
  if(!_expose_global.entered)
    return;

  const xcb_keysym_t keysym = unagi_key_getkeysym(event->detail, event->state);
  if(globalconf.crtc_len > 1 &&
     keysym == _expose_global.keys.crtc_cycle)
    {
      unsigned int crtc_i;
      for(crtc_i = 0; crtc_i < globalconf.crtc_len; crtc_i++)
        if(event->root_x >= globalconf.crtc[crtc_i]->x &&
           event->root_x <= (globalconf.crtc[crtc_i]->x +
                             globalconf.crtc[crtc_i]->width) &&
           event->root_y >= globalconf.crtc[crtc_i]->y &&
           event->root_y <= (globalconf.crtc[crtc_i]->y +
                             globalconf.crtc[crtc_i]->height))
          break;

      // Move the pointer to the middle of the first window of the screen
      crtc_i = (crtc_i >= (globalconf.crtc_len - 1)) ? 0 : crtc_i + 1;
      _expose_pointer_move_center(
        _expose_global.crtc_slots[crtc_i].slots[0].scale_window.window);
    }
  else if(keysym == _expose_global.keys.window_up)
    _expose_up_down_update_current_slot(event->root_x, event->root_y, -1);
  else if(keysym == _expose_global.keys.window_prev)
    _expose_previous_next_update_current_slot(event->root_x, event->root_y, -1);
  else if(keysym == _expose_global.keys.window_next)
    _expose_previous_next_update_current_slot(event->root_x, event->root_y, 1);
  else if(keysym == _expose_global.keys.window_down)
    _expose_up_down_update_current_slot(event->root_x, event->root_y, 1);
  else if(keysym == _expose_global.keys.window_select &&
          _expose_update_current_crtc_and_slot(event->root_x, event->root_y))
    _expose_show_selected_window();
  else if(keysym == _expose_global.keys.quit)
    _expose_quit();
}

/** Check whether the given window is within the given coordinates
 *
 * \param x The x coordinate
 * \param y The y coordinate
 * \param window The window object to check for coordinates
 * \return true if the given window is in the given coordinates
 */
static inline bool
_expose_in_window(const int16_t x, const int16_t y,
		  const unagi_window_t *window)
{
  return x >= window->geometry->x &&
    x < (int16_t) (window->geometry->x + window_width_with_border(window->geometry)) &&
    y >= window->geometry->y &&
    y < (int16_t) (window->geometry->y + window_height_with_border(window->geometry));
}

/** Handle X  ButtonRelease event used  when the user choose  a window
 *  among all the thumbnails displayed by the plugin
 *
 * \param event The X buttonRelease event
 */
static void
expose_event_handle_button_release(xcb_button_release_event_t *event,
				   unagi_window_t *unused __attribute__ ((unused)))
{
  if(_expose_global.entered &&
     _expose_update_current_crtc_and_slot(event->root_x, event->root_y))
    _expose_show_selected_window();
}

/** Save the current pointer state at each MotionNotify, so in
 *  pre_repaint() hook, the Window under the last received
 *  MotionNotify will be considered focused and its opacity sets to
 *  opaque
 */
static void
expose_event_handle_motion_notify(xcb_motion_notify_event_t *event,
                                  unagi_window_t *w __attribute__((unused)))
{
  _expose_global.pointer_x = event->root_x;
  _expose_global.pointer_y = event->root_y;
}

/** Convenient  function to  handle X  PropertyNotify event  common to
 *  _NET_CLIENT_LIST, _NET_ACTIVE_WINDOW and _NET_CURRENT_DESKTOP
 *
 * \param get_property_func The function used to send the request to update the atom
 * \param cookie The cookie relative to the request
 */
static inline void
_expose_do_event_handle_property_notify(xcb_get_property_cookie_t (*get_property_func) (xcb_ewmh_connection_t *, int),
					xcb_get_property_cookie_t *cookie)
{
  /* If a request has already  been sent without being retrieved, just
     free it before sending a new one */
  if(cookie->sequence)
    free(xcb_get_property_reply(globalconf.connection, *cookie, NULL));

  *cookie = (*get_property_func)(&globalconf.ewmh, globalconf.screen_nbr);
}				  

/** When   receiving   PropertyNotify  of   either   _NET_CLIENT_LIST,
 *  _NET_ACTIVE_WINDOW or _NET_CURRENT_DESKTOP  Atoms Properties, send
 *  the request  to get the new  value (but do not  retrieve the reply
 *  yet, simply because it is not needed yet)
 *
 * \todo Perhaps it should be handle in the core code for the root window
 * \todo Check the event state
 * \param event The X PropertyNotify event
 */
static void
expose_event_handle_property_notify(xcb_property_notify_event_t *event,
				    unagi_window_t *window __attribute__((unused)))
{
  if(event->atom == globalconf.ewmh._NET_CLIENT_LIST)
    _expose_do_event_handle_property_notify(xcb_ewmh_get_client_list_unchecked,
					    &_expose_global.atoms.client_list_cookie);

  else if(event->atom == globalconf.ewmh._NET_ACTIVE_WINDOW)
    _expose_do_event_handle_property_notify(xcb_ewmh_get_active_window_unchecked,
					    &_expose_global.atoms.active_window_cookie);

  else if(event->atom == globalconf.ewmh._NET_CURRENT_DESKTOP)
    _expose_do_event_handle_property_notify(xcb_ewmh_get_current_desktop_unchecked,
					    &_expose_global.atoms.current_desktop_cookie);
}				    

/** If the Pointer is under a different Window than before, then the
 *  current Window under the Pointer is considered focused (eg opaque
 *  opacity) and added to the global damaged Region and the previously
 *  focused Window is also added to the damaged Region as it is not
 *  opaque anymore
 */
static void
expose_pre_paint(void)
{
  if(!_expose_global.entered)
    return;

  /* This only happens when just entering Expose as GrabPointer is
     issued at that time */
  if(_expose_global.pointer_x == -1 || _expose_global.pointer_y == -1)
    {
      xcb_query_pointer_reply_t *query_pointer_reply =
        xcb_query_pointer_reply(globalconf.connection,
                                xcb_query_pointer_unchecked(globalconf.connection,
                                                            globalconf.screen->root),
                                NULL);

      if(!query_pointer_reply)
        {
          unagi_warn("Cannot get the current Mouse position");
          return;
        }

      _expose_global.pointer_x = query_pointer_reply->root_x;
      _expose_global.pointer_y = query_pointer_reply->root_y;
      free(query_pointer_reply);
    }
  else if(_expose_coordinates_within_slot(_expose_global.current_slot,
                                          _expose_global.pointer_x,
                                          _expose_global.pointer_y))
    return;

  _expose_update_current_crtc_and_slot(_expose_global.pointer_x,
                                       _expose_global.pointer_y);

  unagi_window_t *window_list =
    _expose_global.crtc_slots[0].slots->scale_window.window;

  for(unagi_window_t *window = window_list; window; window = window->next)
    {
      uint16_t new_opacity;

      if(_expose_global.current_slot &&
         _expose_global.current_slot->scale_window.window == window)
        new_opacity = _expose_global.window_opacity.focus;
      else
        new_opacity = _expose_global.window_opacity.unfocus;

      if(new_opacity != window->transform_opacity && !window->damaged)
        {
          window->damaged = true;
          window->damaged_ratio = 1.0;
          unagi_display_add_damaged_region(&window->region, false);
        }

      window->transform_opacity = new_opacity;

      unagi_debug("Window %jx: Set opacity for: opaque=%d, pointer: x=%d, y=%d",
                  window->id,
                  window->transform_opacity == _expose_global.window_opacity.focus,
                  _expose_global.pointer_x, _expose_global.pointer_y);
    }
}

/** Expose-specific optimization: as Windows do not overlap in Expose
 *  (and thus there is no Window compositing required), paint a Window
 *  for next repaint if a DamageNotify event has been received for
 *  that Window, otherwise leave it as it is...
 */
static void
expose_post_paint(void)
{
  if(!_expose_global.entered)
    return;

  for(unagi_window_t *window = _expose_global.crtc_slots[0].slots->scale_window.window;
      window;
      window = window->next)
    {
      window->damaged = false;
      window->damaged_ratio = 0.0;
    }

  unagi_debug("Painting finished");
}

/** Process D-Bus message for org.minidweeb.unagi.plugin.expose D-Bus
 *  Interface, currently only to enter Expose.
 *
 * \see expose_check_requirements
 */
static const char *
expose_dbus_process_message(DBusMessage *msg)
{
  const char *member = dbus_message_get_member(msg);
  if(member == NULL ||
     dbus_message_get_type(msg) != DBUS_MESSAGE_TYPE_METHOD_CALL)
    return DBUS_ERROR_NOT_SUPPORTED;
  else if(strcmp(member, "enter") != 0)
    return DBUS_ERROR_UNKNOWN_METHOD;

  return _expose_enter() ? NULL : DBUS_ERROR_FAILED;
}

/** Called on dlclose() and fee the memory allocated by this plugin */
static void __attribute__((destructor))
expose_destructor(void)
{
  if(globalconf.dbus_connection && plugin_vtable.dbus_process_message)
    unagi_dbus_release_name(_DBUS_NAME);

  if(_expose_global.atoms.client_list)
    {
      xcb_ewmh_get_windows_reply_wipe(_expose_global.atoms.client_list);
      free(_expose_global.atoms.client_list);
    }

  if(_expose_global.atoms.active_window)
    free(_expose_global.atoms.active_window);

  if(_expose_global.atoms.current_desktop)
    free(_expose_global.atoms.current_desktop);

  if(_expose_global.entered)
    _expose_quit();

  cfg_free(_expose_global.cfg);
}

/** Structure holding all the functions addresses */
unagi_plugin_vtable_t plugin_vtable = {
  .name = _PLUGIN_NAME,
  .dbus_process_message = expose_dbus_process_message,
  .events = {
    expose_event_handle_damage_notify,
    NULL,
    NULL,
    expose_event_handle_key_release,
    NULL,
    expose_event_handle_button_release,
    expose_event_handle_motion_notify,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    expose_event_handle_property_notify
  },
  .check_requirements = expose_check_requirements,
  .window_manage_existing = NULL,
  .window_get_opacity = NULL,
  .pre_paint = expose_pre_paint,
  .post_paint = expose_post_paint
};
