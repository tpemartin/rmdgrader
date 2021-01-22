map(
  targetAnswerObjectNames,
  function(x) R.utils::withTimeout(
    {mget_safe(x, envir=envir)},
    timeout = 15)
) -> targetAnswerObjectValues

debug(mget_safe)
mget_safe(targetAnswerObjectNames,
          envir=envir)
R.utils::withTimeout(
  {eval(parse(text=text), envir = envir)},
  timeout = 15
)
