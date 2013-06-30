{application,node_king,
             [{description,"King node"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,mnesia]},
              {mod,{node_king_app,[]}},
              {env,[]},
              {modules,[node_king_app,node_king_conf,node_king_ddb,
                        node_king_sup,node_king_util]}]}.
