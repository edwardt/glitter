{application,glitter,
             [{description,"Erlang interface for gitolite. "},
              {vsn,"0.2"},
              {registered,[glitter]},
              {modules,[app_util,conf_reader,conf_writer,gen_server_cluster,
                        glitter,glitter_app,glitter_conf,glitter_sup]},
              {applications,[kernel,stdlib,sasl,os_mon]},
              {mod,{glitter_app,[]}},
              {env,[]}]}.
