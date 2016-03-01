#include <stdio.h>
#include <malloc.h>
#include <locale.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>

#include <emacs-module.h>

/* cmigemo */
#include "migemo.h"
#include "charset.h"

static int int2char(unsigned int in, unsigned char* out)
{
  switch (in)  {
  case '.': case '*': case '+': case '?':
  case '[': case ']': case '^': case '$':
  case '\\':
    if (!out)
      return 0;
    out[0] = '\\';
    out[1] = in;
    return 2;
  default:
    break;
  }
  return utf8_int2char(in, out);
}

static char *snprintf_realloc(const char *format, ...)
{
  size_t sz = 128, cc;
  char *buf = malloc(sz);

  va_list ap;
  va_start(ap, format);

  while ((cc = vsnprintf(buf, sz, format, ap)) >= sz) {
    va_end(ap);
    char *b = realloc(buf, sz *= 2);
    if (!b) {
      free(buf);
      return NULL;
    }
    buf = b;
    va_start(ap, format);
  }
  va_end(ap);
  if (cc < 0) {
    char *x = snprintf_realloc("%s: format error", format);
    return x ? x : strdup("fatal error in mod-cmigemo");
  }
  return buf;
}

#define ERROR(env, msg, ...) do {					\
    assert (env->non_local_exit_check (env) ==				\
	    emacs_funcall_exit_return);					\
    char *errmsg_ = snprintf_realloc(msg, __VA_ARGS__);			\
    size_t l = strlen(errmsg_);						\
    char *errmsg = strcpy(alloca(l + 1), errmsg_);			\
    free(errmsg_);							\
    env->non_local_exit_signal (env, env->intern (env, "error"),	\
				env->make_string (env, errmsg, l));	\
    return env->intern (env, "nil");					\
  } while (0)

/* get string in default codepage */
#define GET_FILENAME(name, ename) do {				\
    ptrdiff_t l = 0;						\
    env->copy_string_contents(env, ename, NULL, &l);		\
    char *p = alloca(l);					\
    env->copy_string_contents(env, ename, p, &l);		\
    char *current_locale = setlocale(LC_CTYPE, NULL);		\
    setlocale(LC_CTYPE, "ja_JP.UTF-8");				\
    size_t wl = mbstowcs(NULL, p, 0);				\
    if (wl < 0) {						\
      setlocale(LC_CTYPE, current_locale);			\
      ERROR(env, "Can't convert dictionary name to wcs");	\
    }								\
    wchar_t *w = alloca((wl + 1) * sizeof(wchar_t));		\
    mbstowcs(w, p, wl + 1);					\
    setlocale(LC_CTYPE, ""); /* default codepage*/		\
    size_t ml = wcstombs(NULL, w, 0);				\
    name = alloca(ml + 1);					\
    wcstombs(name, w, ml + 1);					\
    setlocale(LC_CTYPE, current_locale);			\
  } while (0)

static emacs_value Fcmigemo_open
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  char *dict = NULL;
  GET_FILENAME(dict, args[0]);

  migemo *pmigemo;
  if (!(pmigemo = migemo_open(dict)) ||  !migemo_is_enable(pmigemo))
    ERROR(env, "%s: C/Migemo open error.", dict);

  migemo_setproc_int2char(pmigemo, int2char);
  migemo_set_operator(pmigemo, MIGEMO_OPINDEX_OR, "\\|");
  migemo_set_operator(pmigemo, MIGEMO_OPINDEX_NEST_IN, "\\(?:");
  migemo_set_operator(pmigemo, MIGEMO_OPINDEX_NEST_OUT, "\\)");
  migemo_set_operator(pmigemo, MIGEMO_OPINDEX_NEWLINE, "\\s-*");

  return env->make_user_ptr(env, migemo_close, pmigemo);
}

static emacs_value Fcmigemo_close
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  migemo *pmigemo = env->get_user_ptr(env, args[0]);
  if (pmigemo)
    return env->intern (env, "nil");
  migemo_close(pmigemo);
  env->set_user_ptr(env, args[0], NULL);
  env->set_user_finalizer(env, args[0], NULL);
  return env->intern (env, "t");
}

static emacs_value Fcmigemo_query
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  migemo *pmigemo = env->get_user_ptr(env, args[0]);
  if (!pmigemo)
    ERROR(env, "cmigemo already closed");

  ptrdiff_t l = 0;
  env->copy_string_contents(env, args[1], NULL, &l);
  char *word = alloca(l);
  env->copy_string_contents(env, args[1], word, &l);

  char *ans;
  if (ans = migemo_query(pmigemo, word)) {
    emacs_value r =  env->make_string(env, ans, strlen(ans));
    migemo_release(pmigemo, ans);
    return r;
  } else
    return env->intern (env, "nil");
}

static emacs_value Fcmigemo_load
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  migemo *pmigemo = env->get_user_ptr(env, args[0]);
  if (!pmigemo)
    ERROR(env, "cmigemo already closed");

  char *dict;
  GET_FILENAME(dict, args[1]);

  if (migemo_load(pmigemo, MIGEMO_DICTID_MIGEMO,
		  dict) == MIGEMO_DICTID_INVALID)
    ERROR(env, "migemo_load invalid dict");

  /*
   * Migemo_load resets int2char proc,
   * then we set it again.
   */
  migemo_setproc_int2char(pmigemo, int2char);
  return env->intern (env, "t");
}

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}
/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("cmigemo-open", Fcmigemo_open, 1, 1,
	 "open cmigemo with DICTIONARY", NULL);
  DEFUN ("cmigemo-close", Fcmigemo_close, 1, 1,
	 "close cmigemo", NULL);
  DEFUN ("cmigemo-query", Fcmigemo_query, 2, 2,
	 "query cmigemo about WORD", NULL);
  DEFUN ("cmigemo-load", Fcmigemo_load, 2, 2,
	 "load a sub DICTIONARY.", NULL);

  provide(env, "mod-cmigemo");
  return 0;
}

#ifdef _MSC_VER
__declspec(dllexport)
#endif
int plugin_is_GPL_compatible;
