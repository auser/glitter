{application, glitter,
 [
  {description, "glitter - Erlang interface for gitolite configs"},
  {vsn, "0.2"},
  {id, "glitter"},
  {modules,      
  [
  conf_reader,
  conf_writer,
  glitter,
  glitter_app,
  glitter_sup,
  make_boot
  ]},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {glitter_app, []}},
 ]
}.
