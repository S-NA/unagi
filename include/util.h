/* -*-mode:c;coding:utf-8; c-basic-offset:2;fill-column:70;c-file-style:"gnu"-*-
 *
 * Copyright (C) 2012 Gaël Le Mignot <kilobug@kilobug.org>
 *               2009-2012 Arnaud "arnau" Fontaine <arnau@mini-dweeb.org>
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
 *  \brief Miscellaneous helpers not related to X
 */

#ifndef UNAGI_UTIL_H
#define UNAGI_UTIL_H

#include <system.h>
#include <stdint.h>

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#define mod(x, N) ((((x) < 0) ? (((x) % (N)) + (N)) : (x)) % (N))
#define min(x, y) ((x) < (y) ? (x) : (y))

#define unagi_ssizeof(foo)            (ssize_t)sizeof(foo)
#define unagi_countof(foo)            (unagi_ssizeof(foo) / unagi_ssizeof(foo[0]))

#define unagi_fatal(string, ...) _unagi_fatal(true,                     \
                                              __LINE__, __FUNCTION__,	\
                                              string, ## __VA_ARGS__)

#define unagi_fatal_no_exit(string, ...)                                \
  _unagi_fatal(false, __LINE__, __FUNCTION__, string, ## __VA_ARGS__)

void _unagi_fatal(bool, int, const char *, const char *, ...)   \
  __attribute__ ((format(printf, 4, 5)));

#define unagi_warn(string, ...) _unagi_warn(__LINE__,                   \
                                            __FUNCTION__,               \
                                            string, ## __VA_ARGS__)

void _unagi_warn(int, const char *, const char *, ...)
  __attribute__ ((format(printf, 3, 4)));

#define unagi_info(string, ...) _unagi_info(__LINE__,                   \
                                            __FUNCTION__,               \
                                            string, ## __VA_ARGS__)

void _unagi_info(int, const char *, const char *, ...)
  __attribute__ ((format(printf, 3, 4)));

#define unagi_debug(string, ...) _unagi_debug(__LINE__,                 \
                                              __FUNCTION__,             \
                                              string, ## __VA_ARGS__)

void _unagi_debug(int, const char *, const char *, ...)
  __attribute__ ((format(printf, 3, 4)));

#define unagi_util_free(mem_pp)			   \
  {						   \
    typeof(**(mem_pp)) **__ptr = (mem_pp);         \
    free(*__ptr);                                  \
    *__ptr = NULL;                                 \
  }

char *unagi_util_get_configuration_filename_path(const char *);

typedef struct _unagi_util_itree_t
{
  uint32_t key;
  int height;
  void *value;
  struct _unagi_util_itree_t *left;
  struct _unagi_util_itree_t *right;
  struct _unagi_util_itree_t *parent;
} unagi_util_itree_t;

unagi_util_itree_t *util_itree_new(void);
unagi_util_itree_t *util_itree_insert(unagi_util_itree_t *, uint32_t, void *);
void *util_itree_get(unagi_util_itree_t *, uint32_t);
unagi_util_itree_t *util_itree_remove(unagi_util_itree_t *, uint32_t);
uint32_t util_itree_size(unagi_util_itree_t *);
void unagi_util_itree_free(unagi_util_itree_t *);

#ifdef __DEBUG__
#include <stdio.h>

void unagi_util_itree_print(FILE *, unagi_util_itree_t *);
int unagi_util_itree_check(FILE *, unagi_util_itree_t *);
#endif /* __DEBUG__ */

#endif /* UNAGI_UTIL_H */
