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
  glitter_conf,
  app_util,
  gen_server_cluster
 
  ]},
  {registered,   [glitter]},
  {applications, [kernel, stdlib, sas, os_mon]},
  {mod, {glitter_app, []}}
 ]
}.
