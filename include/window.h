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
 *  \brief Windows management
 */

#ifndef UNAGI_WINDOW_H
#define UNAGI_WINDOW_H

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#include <xcb/xcb.h>
#include <xcb/damage.h>
#include <xcb/xfixes.h>

#include "util.h"

#define UNAGI_WINDOW_FULLY_DAMAGED_RATIO 0.9

#define UNAGI_WINDOW_TRANSFORM_STATUS_NONE 0
#define UNAGI_WINDOW_TRANSFORM_STATUS_REQUIRED 1
#define UNAGI_WINDOW_TRANSFORM_STATUS_DONE 2

typedef struct _unagi_window_t
{
  xcb_window_t id;
  xcb_get_window_attributes_reply_t *attributes;
  xcb_get_geometry_reply_t *geometry;
  xcb_xfixes_region_t region;
  xcb_xfixes_fetch_region_cookie_t shape_cookie;
  bool is_rectangular;
  xcb_damage_damage_t damage;
  bool damaged;
  float damaged_ratio;
  short damage_notify_counter;
  xcb_pixmap_t pixmap;
  int transform_status;
  double transform_matrix[4][4];
  uint16_t transform_opacity;
  void *rendering;
  struct _unagi_window_t *next;
  struct _unagi_window_t *prev;
} unagi_window_t;

void unagi_window_free_pixmap(unagi_window_t *);
void unagi_window_list_cleanup(void);

/** Get the  window object  associated with the  given Window  XID. As
 *  this is a very common operation, use a binary tree rather than the
 *  linked list. The linked list is still useful to get windows sorted
 *  by stacking order
 *
 * \param WINDOW_ID The Window XID to look for
 */
#define unagi_window_list_get(WINDOW_ID) util_itree_get(globalconf.windows_itree, \
                                                        WINDOW_ID)

void unagi_window_list_remove_window(unagi_window_t *, bool);
void unagi_window_register_notify(const unagi_window_t *);
void unagi_window_get_root_background_pixmap(void);
xcb_pixmap_t unagi_window_get_root_background_pixmap_finalise(void);
xcb_pixmap_t unagi_window_new_root_background_pixmap(void);
xcb_pixmap_t unagi_window_get_pixmap(const unagi_window_t *);
bool unagi_window_is_rectangular(unagi_window_t *);
xcb_xfixes_region_t unagi_window_get_region(unagi_window_t *, bool, bool);
bool unagi_window_is_visible(const unagi_window_t *);
void unagi_window_get_invisible_window_pixmap(unagi_window_t *);
void unagi_window_get_invisible_window_pixmap_finalise(unagi_window_t *);
void unagi_window_manage_existing(const int nwindows, const xcb_window_t *);
unagi_window_t *window_add(const xcb_window_t, bool);
void unagi_window_map_raised(const unagi_window_t *);
void unagi_window_restack(unagi_window_t *, xcb_window_t);
void unagi_window_paint_all(unagi_window_t *);

static inline float
window_get_damaged_ratio(unagi_window_t *window, xcb_damage_notify_event_t *event)
{
  window->damaged_ratio += (float) (event->area.width * event->area.height) /
    (float) (window->geometry->width * window->geometry->height);

  return window->damaged_ratio;
}

#define UNAGI_DO_GEOMETRY_WITH_BORDER(kind)                             \
  static inline uint16_t						\
  window_##kind##_with_border(const xcb_get_geometry_reply_t *geometry)	\
  {									\
    return (uint16_t) (geometry->kind + (geometry->border_width * 2));	\
  }

UNAGI_DO_GEOMETRY_WITH_BORDER(width)
UNAGI_DO_GEOMETRY_WITH_BORDER(height)

#endif
