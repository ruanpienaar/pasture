-module (pasture_utils).

-export ([try_get_column/2,
          decode_topics/1]).

try_get_column(Objs,Search) ->
    case lists:keyfind(Search,1,Objs) of
        %% Todo; some value's could be more json...
        %% decode properly...
        {Search,Value}  -> Value;
        false           -> undefined
    end.

decode_topics(undefined) ->
    undefined;
decode_topics(V) ->
    decode_topics(V,[]).

decode_topics([],R) ->
    R;
decode_topics([[{<<"urlkey">>,UrlKey},_]|T],R) ->
    decode_topics(T,[{urlkey,UrlKey}|R]).