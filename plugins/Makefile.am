AM_CPPFLAGS = -I$(top_srcdir)/include $(UNAGI_CFLAGS)

opacity_la_LDFLAGS = -no-undefined -module -avoid-version $(UNAGI_LIBS)
opacity_la_SOURCES = opacity.c
opacity_la_LIBTOOLFLAGS = --tag=disable-static

expose_la_LDFLAGS = -no-undefined -module -avoid-version -lm $(UNAGI_LIBS)
expose_la_SOURCES = expose.c
expose_la_LIBTOOLFLAGS = --tag=disable-static

plugins_LTLIBRARIES = opacity.la expose.la
