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
 *  X and Compiz  (known as Scale plugin) but  is not really optimised
 *  (yet) because it repaints all  the windows even if the content has
 *  not been changed  and it should also maybe  use SHM for xcb_image.
 *  The window  slots could be arranged  in a better  way by including
 *  the window geometry in the computation and the rescaling algorithm
 *  should be improved to decrease the blurry effect.
 *
 *  It relies  on _NET_CLIENT_LIST  (required otherwise the  plugin is
 *  disabled) and _NET_ACTIVE_WINDOW atoms (required too and stored in
 *  '_expose_global.atoms' structure) to  get respectively the clients
 *  managed  by the  window manager  and the  current  focused window.
 *  These atoms values are updated in a lazy way (e.g.  by sending the
 *  GetProperty requests on initialisation and PropertyNotify and then
 *  getting the reply as late as needed).
 *
 *  The rendering is performed in  the following steps when the plugin
 *  is enabled ('_expose_plugin_enable'):
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
 *   4/ For each window, create  a new 'window_t' object which will be
 *      then given to 'window_paint_all' function of the core code. If
 *      the window  needs to be  rescaled (e.g.  when the  window does
 *      not  fit  the  slot),  create  a  new  Image,  Pixmap  and  GC
 *      ('_expose_prepare_windows'). Then (and each time the window is
 *      repainted), get the Image of  the original window and get each
 *      pixel  which   will  then  be   put  on  the   rescaled  Image
 *      ('_expose_update_scale_pixmap')  using  a Gaussian-filter-like
 *      rescaled algorithm.
 */

#include <math.h>
#include <string.h>
#include <stdlib.h>

#include <X11/keysym.h>

#include <xcb/xcb.h>
#include <xcb/xcb_ewmh.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_aux.h>

#include "structs.h"
#include "window.h"
#include "plugin.h"
#include "atoms.h"
#include "util.h"
#include "key.h"
#include "event.h"

/** Activation Keysym
 * \todo Remove
 */
#define PLUGIN_KEY XK_F12

/** Spacing between thumbnails
 * \todo Remove
 */
#define STRIP_SPACING 10

/** Weights values apply  to pixels around a given  pixel value (whose
    weight is center of the matrice) */
static const uint32_t _expose_scale_weights[][3] = {
  { 1, 4, 1},
  { 4, 10, 4},
  { 1, 4, 1}
};

/** Get each channel component of a 32-bit pixel */
#define GET_R(pixel, weight) (((pixel) & 0x0000ff) * weight)
#define GET_G(pixel, weight) GET_R(pixel >> 8, weight)
#define GET_B(pixel, weight) GET_R(pixel >> 16, weight)

/** From each  channel component, compute  the actual pixel  value but
    multiply each channel by a weight */
#define SET_PIXEL(r, g, b, w) ((r / w) | ((g / w) << 8) | ((b / w) << 16))

/** Expose window */
typedef struct
{
  /** Rescaled window */
  window_t *window;
  /** Rescaled image of the window */
  xcb_image_t *image;
  /** Graphical context of the rescaled window pixmap */
  xcb_gcontext_t gc;
  /** If the window was unmapped before enabling the plugin */
  bool was_unmapped;
} _expose_scale_window_t;

/** Each window is contained within a slot */
typedef struct
{
  /** Slot geometry */
  xcb_rectangle_t extents;
  /** Nearest window associated with this slot */
  window_t *window;
  /** Rescaled window */
  _expose_scale_window_t scale_window;
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
} _expose_atoms_t;

/** Global variables of this plugin */
static struct
{
  /** Is the plugin enabled */
  bool enabled;
  /** Atoms structure */
  _expose_atoms_t atoms;
  /** Slots for thumbnails */
  _expose_window_slot_t *slots;
} _expose_global;

/** Called  on  dlopen() to  initialise  memory  areas  and also  send
 *  GetProperty requests  on the root window  for _NET_CLIENT_LIST and
 *  _NET_ACTIVE_WINDOW atoms to avoid  blocking when these values will
 *  be needed
 */
static void __attribute__((constructor))
expose_constructor(void)
{
  memset(&_expose_global, 0, sizeof(_expose_global));

  _expose_global.atoms.client_list = NULL;
  _expose_global.atoms.active_window = NULL;

  /* Send  the requests  to check  whether the  atoms are  present and
     whose replies will  be got when actually calling  the function to
     check atoms which are required */
  _expose_global.atoms.client_list_cookie =
    xcb_ewmh_get_client_list_unchecked(&globalconf.ewmh,
                                       globalconf.screen_nbr);

  _expose_global.atoms.active_window_cookie =
    xcb_ewmh_get_active_window_unchecked(&globalconf.ewmh,
                                         globalconf.screen_nbr);
}

/** Update   the    values   of   _NET_CLIENT_LIST    (required)   and
 *  _NET_ACTIVE_WINDOW (required) if the GetProperty has been sent but
 *  not  already  retrieved  (thus  on  plugin  initialisation  or  on
 *  PropertyNotify event).  This function  also frees the slots if the
 *  clients list has to be updated
 *
 * \param atoms Atoms information
 * \param slots The slots to be freed if necessary
 */
static void
_expose_update_atoms_values(_expose_atoms_t *atoms,
			    _expose_window_slot_t **slots)
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
	  warn("Can't get %s: plugin disabled for now", #atom_name);	\
	  util_free(&atoms->kind);					\
	}								\
									\
      /* Reset the cookie sequence for the next request */		\
      atoms->kind##_cookie.sequence = 0;				\
    }

  CHECK_REQUIRED_ATOM(client_list, xcb_ewmh_get_windows_reply_t, _NET_CLIENT_LIST)
  CHECK_REQUIRED_ATOM(active_window, xcb_window_t, _NET_ACTIVE_WINDOW)
}

/** Check  whether  the plugin  can  actually  be  enabled, only  when
 *  _NET_CLIENT_LIST Atom Property has been set on the root window
 *
 * \todo make the GrabKey completely asynchronous
 * \return true if the plugin can be enabled
 */
static bool
expose_check_requirements(void)
{
  if(!atoms_is_supported(globalconf.ewmh._NET_CLIENT_LIST))
    return false;

  _expose_update_atoms_values(&_expose_global.atoms, &_expose_global.slots);
  if(!_expose_global.atoms.client_list || !_expose_global.atoms.active_window)
    return false;

  /* Send the GrabKey request on the key given in the configuration */
  xcb_keycode_t *keycode = keycode = xcb_key_symbols_get_keycode(globalconf.keysyms,
								 PLUGIN_KEY);

  xcb_void_cookie_t grab_key_cookie = xcb_grab_key_checked(globalconf.connection, false,
							   globalconf.screen->root,
							   XCB_NONE, *keycode,
							   XCB_GRAB_MODE_ASYNC,
							   XCB_GRAB_MODE_ASYNC);

  free(keycode);

  /* Check  whether the GrabKey  request succeeded,  otherwise disable
     the plugin */
  xcb_generic_error_t *error = xcb_request_check(globalconf.connection,
						 grab_key_cookie);

  if(error)
    {
      warn("Can't grab selected key");
      free(error);
      exit(EXIT_FAILURE);
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

/** Create the slots where the  window will be arranged. The screen is
 *  divided in  strips of the same  size whose number is  given by the
 *  square root of the number of windows
 *
 * \param nwindows The number of windows
 * \param nwindows_per_strip The number of windows per strip
 * \return The newly allocated slots
 */
static void
_expose_create_slots(const uint32_t nwindows, unsigned int *nwindows_per_strip)
{
  _expose_global.slots = calloc(nwindows + 1, sizeof(_expose_window_slot_t));
  if(!_expose_global.slots)
    return;

  /* The  screen is  divided  in  strips depending  on  the number  of
     windows */
  const uint8_t strips_nb = (uint8_t) sqrt(nwindows + 1);

  /* Each strip height excludes spacing */
  const uint16_t strip_height = (uint16_t) 
    ((globalconf.screen->height_in_pixels - STRIP_SPACING * (strips_nb + 1)) / strips_nb);

  /* The number of windows per strip depends */
  *nwindows_per_strip = (unsigned int) ceilf((float) nwindows / (float) strips_nb);

  int16_t current_y = STRIP_SPACING, current_x;

  /* Each slot is a rectangle  whose coordinates depends on the number
     of strips and the number of windows */	
  unsigned int slot_n = 0;

  /* Create the strips of windows */
  for(uint8_t strip_n = 0; strip_n < strips_nb; strip_n++)
    {
      current_x = STRIP_SPACING;

      /* Number of slots for this strip which depends on the number of
	 remaining slots (the last strip may contain less windows) */
      const unsigned int strip_slots_n =
	(nwindows - slot_n > *nwindows_per_strip ? *nwindows_per_strip : nwindows - slot_n);

      /* Slot width including spacing */
      const uint16_t slot_width = (uint16_t)
	((globalconf.screen->width_in_pixels - STRIP_SPACING * (strip_slots_n + 1)) / strip_slots_n);

      /* Now create the slots associated to this strip */
      for(unsigned int strip_slot = 0; strip_slot < strip_slots_n; strip_slot++)
	{
	  _expose_global.slots[slot_n].extents.x = current_x;
	  _expose_global.slots[slot_n].extents.y = current_y;
	  _expose_global.slots[slot_n].extents.width = slot_width;
	  _expose_global.slots[slot_n].extents.height = strip_height;

	  current_x = (int16_t) (current_x + slot_width + STRIP_SPACING);
	  ++slot_n;
	}

      current_y = (int16_t) (current_y + strip_height + STRIP_SPACING);
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
_expose_assign_windows_to_slots(const uint32_t nwindows,
				const uint32_t nwindows_per_strip,
				_expose_window_slot_t *slots)
{
  struct
  {
    window_t *window;
    /* Coordinates of the window center */
    int16_t x, y;
  } windows[nwindows];

  /* Prepare the  windows and their information  before assigning them
     to a slot */
  for(uint32_t window_n = 0; window_n < nwindows; window_n++)
    {
      window_t *window = window_list_get(_expose_global.atoms.client_list->windows[window_n]);

      windows[window_n].window = window;
      windows[window_n].x = (int16_t) (window->geometry->x + window->geometry->width / 2);
      windows[window_n].y = (int16_t) (window->geometry->y + window->geometry->height / 2);
    }

  /* Assign the windows to its slot using Euclidian distance */
  for(uint32_t slot_n = 0; slot_n < nwindows; slot_n++)
    {
      const int16_t slot_x = (int16_t) (slots[slot_n].extents.x +
					slots[slot_n].extents.width / 2);

      const int16_t slot_y = (int16_t) (slots[slot_n].extents.y +
					slots[slot_n].extents.height / 2);

      int16_t x, y;
      uint16_t distance, nearest_distance = UINT16_MAX;
      uint32_t window_n_nearest = 0;

      for(uint32_t window_n = 0; window_n < nwindows; window_n++)
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
  for(uint32_t slot_n = 0; slot_n < nwindows; slot_n += nwindows_per_strip)
    {
      /* Number of spare pixels */
      unsigned int slot_spare_pixels = 0;
      /* Number of slots to extend */
      unsigned int slots_to_extend_n = 0;

      for(uint32_t window_strip_n = 0; window_strip_n < nwindows_per_strip;
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
      if(slots_to_extend_n)
	continue;

      uint16_t spare_pixels_per_slot = (uint16_t) (slot_spare_pixels / slots_to_extend_n);

      for(uint32_t window_strip_n = 0; window_strip_n < nwindows_per_strip;
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
 */
static void
_expose_prepare_windows(void)
{
  window_t *scale_window_prev = NULL;

  for(_expose_window_slot_t *slot = _expose_global.slots;
      slot && slot->window; slot++)
    {
      const uint16_t window_width = window_width_with_border(slot->window->geometry);
      const uint16_t window_height = window_height_with_border(slot->window->geometry);
      window_t *scale_window;

      /* If the window does not need to be rescaled, just use existing window */
      if(!_expose_window_need_rescaling(&slot->extents, window_width, window_height))
	{
	  debug("No need to scale %jx", (uintmax_t) slot->window->id);
          scale_window = malloc(sizeof(window_t));
          memcpy(scale_window, slot->window, sizeof(window_t));
          scale_window->next = NULL;
	}
      else
        {
          scale_window = calloc(1, sizeof(window_t));
          scale_window->id = slot->window->id;
          scale_window->attributes = slot->window->attributes;
          scale_window->rendering = slot->window->rendering;
          /* The Pixmap is needed for previously unmapped windows to
             create the Picture for example with Render */
          scale_window->pixmap = slot->window->pixmap;

          /* The scale window coordinates are the slot ones */
          scale_window->geometry = calloc(1, sizeof(xcb_get_geometry_reply_t));
          scale_window->geometry->x = slot->extents.x;
          scale_window->geometry->y = slot->extents.y;
          /* Border width is always equals to 0 as it is scaled anyway */
          scale_window->geometry->border_width = 0;

          /* Compute the ratio from the  largest side (width or height) of
             the window */
          const float ratio =
            ((window_width - slot->extents.width) >
             (window_height - slot->extents.height)) ?
            (float) slot->extents.width / (float) window_width :
            (float) slot->extents.height / (float) window_height;

          scale_window->geometry->width = (uint16_t)
            floorf(ratio * (float) window_width);

          scale_window->geometry->height = (uint16_t)
            floorf(ratio * (float) window_height);

          memset(scale_window->transform_matrix, 0, 16);
          scale_window->transform_matrix[0][0] = 1;
          scale_window->transform_matrix[1][1] = 1;
          scale_window->transform_matrix[2][2] = ratio;

          scale_window->transform_status = WINDOW_TRANSFORM_STATUS_REQUIRED;
        }

      scale_window->damaged = true;

      /* Link the previous element with the current one */
      if(scale_window_prev)
	scale_window_prev->next = scale_window;

      scale_window_prev = scale_window;
      slot->scale_window.window = scale_window;
    }

#ifdef __DEBUG__
  for(_expose_window_slot_t *slot = slots; slot && slot->window; slot++)
    {
      debug("slot: x=%jd, y=%jd, width=%ju, height=%ju",
	    (intmax_t) slot->extents.x, (intmax_t) slot->extents.y,
	    (uintmax_t) slot->extents.width, (uintmax_t) slot->extents.height);

      debug("scale_window: id=%jx, x=%jd, y=%jd, width=%ju, height=%ju",
	    (uintmax_t) slot->scale_window.window->id,
	    (intmax_t) slot->scale_window.window->geometry->x,
	    (intmax_t) slot->scale_window.window->geometry->y,
	    (uintmax_t) slot->scale_window.window->geometry->width,
	    (uintmax_t) slot->scale_window.window->geometry->height);
    }
#endif
}

/** Disable the  plugin by unmapping  the windows which  were unmapped
 *  before enabling the plugin and then repaint the screen again
 *
 * \param slots The windows slots
 */
static void
_expose_plugin_disable(void)
{
  /* Now ungrab both the keyboard and the pointer */
  xcb_ungrab_keyboard(globalconf.connection, XCB_CURRENT_TIME);
  xcb_ungrab_pointer(globalconf.connection, XCB_CURRENT_TIME);

  for(_expose_window_slot_t *slot = _expose_global.slots;
      slot && slot->window; slot++)
    {
      /* Unmap the window which were previously mapped and also
         restore override redirect */
      if(slot->scale_window.was_unmapped)
        window_get_invisible_window_pixmap_finalise(slot->window);

      /* Free memory allocated only for Windows *actually* scaled */
      if(slot->scale_window.window->transform_status != WINDOW_TRANSFORM_STATUS_NONE)
        {
          (*globalconf.rendering->free_window)(slot->window);
          util_free(&(slot->scale_window.window->geometry));
          util_free(&(slot->scale_window.window));
        }
    }

  util_free(&_expose_global.slots);
  _expose_global.enabled = false;

  /* Force repaint of the screen as the plugin is now disabled */
  globalconf.force_repaint = true;
}

/** Enable  the plugin  by  creating  the windows  slots  and map  the
 *  windows which are not already mapped, then fits the windows in the
 *  slots and create their Pixmap, and finally repaint the screen
 *
 * \param nwindows The numbers of windows on the screen
 * \return The newly allocated slots
 */
static void
_expose_plugin_enable(const uint32_t nwindows)
{
  unsigned int nwindows_per_strip;

  _expose_create_slots(nwindows, &nwindows_per_strip);
  if(!_expose_global.slots)
    return;

  _expose_assign_windows_to_slots(nwindows, nwindows_per_strip,
                                  _expose_global.slots);

  xcb_grab_server(globalconf.connection);

  /* Map windows which where  unmapped otherwise the window content is
     not guaranteed to be preserved while the window is unmapped */
  for(_expose_window_slot_t *slot = _expose_global.slots; slot && slot->window; slot++)
    if(slot->window->attributes->map_state != XCB_MAP_STATE_VIEWABLE &&
       !slot->scale_window.was_unmapped)
      {
	window_get_invisible_window_pixmap(slot->window);
	slot->scale_window.was_unmapped = true;
      }

  /** Process MapNotify event to get the NameWindowPixmap
   *  \todo get only MapNotify? */
  xcb_aux_sync(globalconf.connection);
  
  event_handle_poll_loop(event_handle);

  xcb_ungrab_server(globalconf.connection);

  /** Grab the pointer in an  active way to avoid EnterNotify event due
   *  to the mapping hack
   *
   *  \todo improve focus handling
   */
  xcb_grab_pointer_cookie_t grab_pointer_cookie =
    xcb_grab_pointer_unchecked(globalconf.connection, true, globalconf.screen->root,
			       XCB_EVENT_MASK_BUTTON_RELEASE, XCB_GRAB_MODE_ASYNC,
			       XCB_GRAB_MODE_ASYNC, XCB_NONE, XCB_NONE,
			       XCB_CURRENT_TIME);

  /* Grab  the keyboard  in an  active way  to avoid  "weird" behavior
     (e.g. being  able to type in  a window which may  be not selected
     due  to  rescaling)  due   to  the  hack  consisting  in  mapping
     previously unmapped windows to get their Pixmap */
  xcb_grab_keyboard_cookie_t grab_keyboard_cookie =
    xcb_grab_keyboard_unchecked(globalconf.connection, true, globalconf.screen->root,
				XCB_CURRENT_TIME, XCB_GRAB_MODE_ASYNC,
				XCB_GRAB_MODE_ASYNC);
  
  _expose_prepare_windows();

  xcb_grab_pointer_reply_t *grab_pointer_reply =
    xcb_grab_pointer_reply(globalconf.connection, grab_pointer_cookie, NULL);

  xcb_grab_keyboard_reply_t *grab_keyboard_reply =
    xcb_grab_keyboard_reply(globalconf.connection, grab_keyboard_cookie, NULL);

  if(!grab_pointer_reply || grab_pointer_reply->status != XCB_GRAB_STATUS_SUCCESS ||
     !grab_keyboard_reply || grab_keyboard_reply->status != XCB_GRAB_STATUS_SUCCESS)
    {
      warn("Can't grab the pointer and/or the keyboard");
      _expose_plugin_disable();
    }

  free(grab_pointer_reply);
  free(grab_keyboard_reply);

  /* TODO: Really bad from a performance point of view */
  globalconf.force_repaint = true;
}

/** When receiving a KeyRelease  event, just enable/disable the plugin
 *  if the plugin shortcuts key has been pressed and released
 *
 * \param event The X KeyPress event
 */
static void
expose_event_handle_key_release(xcb_key_release_event_t *event,
				window_t *window __attribute__((unused)))
{
  if(key_getkeysym(event->detail, event->state) != PLUGIN_KEY)
    return;

  if(_expose_global.enabled)
    _expose_plugin_disable();
  else
    {
      /* Update the  atoms values  now if it  has been changed  in the
	 meantime */
      _expose_update_atoms_values(&_expose_global.atoms, &_expose_global.slots);

      /* Get  the number  of windows  actually managed  by  the window
	 manager (as given by _NET_CLIENT_LIST) */
      const uint32_t nwindows = _expose_global.atoms.client_list->windows_len;
      if(nwindows)
	{
	  _expose_plugin_enable(nwindows);
	  if(!_expose_global.slots)
	    warn("Couldn't create the slots: Not enabled");
	  else
	    _expose_global.enabled = true;
	}
    }
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
		  const window_t *window)
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
				   window_t *window __attribute__ ((unused)))
{
  for(uint32_t window_n = 0;
      window_n < _expose_global.atoms.client_list->windows_len;
      window_n++)
    {
      if(_expose_in_window(event->root_x, event->root_y,
			   _expose_global.slots[window_n].scale_window.window))
	{
	  _expose_plugin_disable(_expose_global.slots);

	  xcb_ewmh_request_change_active_window(&globalconf.ewmh, globalconf.screen_nbr,
						_expose_global.slots[window_n].window->id,
						XCB_EWMH_CLIENT_SOURCE_TYPE_OTHER,
						event->time, XCB_NONE);

	  break;
	}
    }
}

/** Convenient  function to  handle X  PropertyNotify event  common to
 *  _NET_CLIENT_LIST and _NET_ACTIVE_WINDOW
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

/** When  receiving  PropertyNotify   of  either  _NET_CLIENT_LIST  or
 *  _NET_ACTIVE_WINDOW Atoms  Properties, send the request  to get the
 *  new value (but do not retrieve the reply yet, simply because it is
 *  not needed yet)
 *
 * \todo Perhaps it should be handle in the core code for the root window
 * \todo Check the event state
 * \param event The X PropertyNotify event
 */
static void
expose_event_handle_property_notify(xcb_property_notify_event_t *event,
				    window_t *window __attribute__((unused)))
{
  if(event->atom == globalconf.ewmh._NET_CLIENT_LIST)
    _expose_do_event_handle_property_notify(xcb_ewmh_get_client_list_unchecked,
					    &_expose_global.atoms.client_list_cookie);

  else if(event->atom == globalconf.ewmh._NET_ACTIVE_WINDOW)
    _expose_do_event_handle_property_notify(xcb_ewmh_get_active_window_unchecked,
					    &_expose_global.atoms.active_window_cookie);
}				    

/** If the plugin is enabled, update the scaled Pixmap and then return
 *  the scaled windows objects
 *
 * \return The beginning of the scaled windows list
 */
static window_t *
expose_render_windows(void)
{
  if(!_expose_global.enabled)
    return NULL;

  for(_expose_window_slot_t *slot = _expose_global.slots; slot && slot->window; slot++)
    slot->scale_window.window->damaged = true;

  globalconf.force_repaint = true;
  return _expose_global.slots[0].scale_window.window;
}

/** Called on dlclose() and fee the memory allocated by this plugin */
static void __attribute__((destructor))
expose_destructor(void)
{
  if(_expose_global.atoms.client_list)
    {
      xcb_ewmh_get_windows_reply_wipe(_expose_global.atoms.client_list);
      free(_expose_global.atoms.client_list);
    }

  free(_expose_global.atoms.active_window);
  _expose_plugin_disable();
}

/** Structure holding all the functions addresses */
plugin_vtable_t plugin_vtable = {
  .name = "expose",
  .events = {
    NULL,
    NULL,
    NULL,
    expose_event_handle_key_release,
    expose_event_handle_button_release,
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
  .render_windows = expose_render_windows
};
