{application, glitter,
 [
  {description, "glitter app"},
  {vsn, "0.1"},
  {id, "glitter"},
  {modules,      []},
  {registered,   []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {glitter_app, []}},
  {env, [
    {config_file, "env/gitosis.conf"}
  ]}
 ]
}.