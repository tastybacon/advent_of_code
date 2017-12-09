-module(recursive_circus).
-export([read_input_file/1, find_base/1]).

% Input parsing

read_input_file(Filename) ->
	{ok, Data} = file:read_file(Filename),
  lists:map(fun parse_row/1,
            binary:split(Data, [<<"\n">>], [global, trim])).

parse_row(Row) ->
  MatchData = re:run(
             Row,
             "\\A(\\w*) \\((\\d+)\\)(?: -> (.*))?\\Z",
             [{capture, all_but_first, list}]
            ),
  case MatchData of
    {match, [Id, Weight, Children]} -> new_node(Id, Weight, string:tokens(Children, ", "));
    {match, [Id, Weight]} -> new_node(Id, Weight);
    nomatch -> none
  end.

% Node stuff

new_node(Id, Weight) ->
  {node, Id, Weight, []}.
new_node(Id, Weight, Children) ->
  {node, Id, Weight, Children}.

is_child(Id, {node, _, _, Children}) ->
  lists:member(
    Id,
    lists:map(fun({node, ChildId, _, _}) -> ChildId end, Children)
   ).

is_descendent(Id, {node, _, _, Children} = Node) ->
  case is_child(Id, Node) of
    true -> true;
    false -> lists:any(fun(X) -> is_child(Id, X) end, Children)
  end.

% Tower Structure

find_base(Nodes) ->
  find_base(Nodes, sets:new(), sets:new()).

find_base([], AllNodes, ChildNodes) ->
  [Base] = sets:to_list(sets:subtract(AllNodes, ChildNodes)),
  Base;
find_base([{node, Id, _, Children}|Rest], AllNodes, ChildNodes) ->
  find_base(
    Rest,
    sets:add_element(Id, AllNodes),
    sets:union([ChildNodes, sets:from_list(Children)])
   ).
