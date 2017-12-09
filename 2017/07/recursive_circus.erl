-module(recursive_circus).
-export(
   [
    read_input_file/1,
    find_base/1,
    build_tower/1,
    find_unbalanced/1
   ]
  ).

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
    {match, [Id, Weight, Children]} ->
      {IWeight, _} = string:to_integer(Weight),
      new_node(Id, IWeight, string:tokens(Children, ", "));
    {match, [Id, Weight]} ->
      {IWeight, _} = string:to_integer(Weight),
      new_node(Id, IWeight);
    nomatch -> none
  end.

% Node stuff

new_node(Id, Weight) ->
  {node, Id, Weight, []}.
new_node(Id, Weight, Children) ->
  {node, Id, Weight, Children}.

node_weight({node, _, Weight, []}) ->
  Weight;
node_weight({node, _, Weight, Children}) ->
  Weight + lists:sum(lists:map(fun node_weight/1, Children)).

is_balanced({node, _, _, Children}) ->
  case sets:size(sets:from_list(lists:map(fun node_weight/1, Children))) of
    1 -> true;
    _ -> false
  end.

is_unbalanced(Node) ->
  not is_balanced(Node).

find_unbalanced({node, _, _, Children}) ->
  case lists:filter(fun is_unbalanced/1, Children) of
    [Unbalanced] -> find_unbalanced(Unbalanced);
    [] -> lists:map(fun({node, Id, Weight, _} = Node) -> {Id, Weight, node_weight(Node)} end, Children)
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

build_tower(Nodes) ->
  NodeMap = node_list_to_map(Nodes),
  BaseNodeId = find_base(Nodes),
  [BaseNode] = dict:fetch(BaseNodeId, NodeMap),
  build_tower(BaseNode, dict:erase(BaseNodeId, NodeMap)).

build_tower({node, _, _, []} = Tower, _NodeMap) ->
  Tower;
build_tower({node, Id, Weight, Children}, NodeMap) ->
  {node, Id, Weight, lists:map(fun(X) -> build_tower(X, NodeMap) end, Children)};
build_tower(NodeId, NodeMap) ->
  [Node] = dict:fetch(NodeId, NodeMap),
  build_tower(Node, NodeMap).

% Utility

node_list_to_map(Nodes) ->
  node_list_to_map(Nodes, dict:new()).

node_list_to_map([], Dict) ->
  Dict;
node_list_to_map([{node, Id, _, _} = H|T], Dict) ->
  node_list_to_map(T, dict:append(Id, H, Dict)).
