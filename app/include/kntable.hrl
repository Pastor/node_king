-type hostname() :: string().
-type uuid() :: string().
-type node_type() :: 1..10.
-type port_type() :: 1024..65535.

-record(knode, {
                uuid  :: uuid(),
                name  :: string(),
                host  :: hostname(),
                port  :: port_type(),
                type  :: node_type(),
                id    :: number(),
                self  :: boolean(),
                alive :: boolean()
}).

