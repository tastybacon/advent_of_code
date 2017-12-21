-module(particle_swarm).
-compile(export_all).

% {vector, X, Y, Z}
% {particle, Acceleration, Velocity, Position}

read_input(Filename) ->
	{ok, Data} = file:read_file(Filename),
  Particles = binary:split(Data, [<<"\n">>], [global, trim_all]),
  {Parsed, _ } = lists:mapfoldl(
                   fun(P, Index) ->
                       {parse_raw_particle(Index, P), Index + 1}
                   end,
                   0,
                   Particles
                  ),
  Parsed.

parse_raw_particle(Index, ParticleString) ->
  MatchData = re:run(
             ParticleString,
             "\\Ap=< ?(.*)>, v=< ?(.*)>, a=< ?(.*)>\\Z",
             [{capture, all_but_first, list}]
            ),
  case MatchData of
    {match, [Position, Velocity, Acceleration]} ->
      {
       particle,
       Index,
       parse_vector(Position),
       parse_vector(Velocity),
       parse_vector(Acceleration)
      };
    nomatch -> none
  end.

parse_vector(VectorString) ->
  [X, Y, Z] = lists:map(
    fun(X) -> {Int, _} = string:to_integer(X), Int end,
    string:tokens(VectorString, ",")
   ),
  {vector, X, Y, Z}.

closest_to_origin(Particles) ->
  [H|_] = lists:sort(fun compare_particles/2, Particles),
  H.

closest_to_originX(Particles) ->
  lists:foldl(
    fun({particle, _, _, _, A} = P, {particle, _, _, _, B} = Acc) ->
        case manhattan_distance(A, origin()) > manhattan_distance(B, origin()) of
          true -> Acc;
          false -> P
        end;
       ({particle, _, _, _, _} = P, none) -> P
    end,
    none,
    Particles
   ).

origin() ->
  {vector, 0, 0, 0}.

manhattan_distance({vector, X1, Y1, Z1}, {vector, X2, Y2, Z2}) ->
  abs(X2 - X1) + abs(Y2 - Y1) + abs(Z2 - Z1).

add_vector({vector, X1, Y1, Z1}, {vector, X2, Y2, Z2}) ->
  {vector, X1 + X2, Y1 + Y2, Z1 + Z2}.

compare_vector_magnitude({vector, _, _, _} = A, {vector, _, _, _} = B) ->
  manhattan_distance(A, origin()) - manhattan_distance(B, origin()).

compare_particles({particle, _, P1, V1, A1}, {particle, _, P2, V2, A2}) ->
  case compare_vector_magnitude(A1, A2) of
    0 -> case compare_vector_magnitude(V1, V2) of
           0 -> compare_vector_magnitude(P1, P2) < 0;
           X -> X < 0
         end;
    X -> X < 0
  end.

remove_colliding(Particles) ->
  remove_colliding(Particles, dict:new(), sets:new()).

remove_colliding([P|Rest], Dict, Ids) ->
  {particle, Id, _, _, _} = P,
  Points = calculate_points(1000, P),
  NewDict = dict:merge(
              fun(_, A, B) -> A ++ B end,
              dict:from_list(lists:map(fun(X) -> {X, [Id]} end, Points)),
              Dict
             ),
  remove_colliding(Rest, NewDict, sets:add_element(Id,Ids));
remove_colliding([], Dict, Ids) ->
  CollidedEntries = dict:filter(fun(_, V) -> length(V) > 1 end, Dict),
  CollidedIds = lists:foldl(
                  fun({_, X}, A) -> sets:union(sets:from_list(X), A) end,
                  sets:new(),
                  dict:to_list(CollidedEntries)
                 ),
  sets:subtract(Ids, CollidedIds).

calculate_points(Remaining, Particle) ->
  calculate_points(Remaining, Particle, []).

calculate_points(0, {particle, _, P, _, _}, Acc) ->
  [P|Acc];
calculate_points(Remaining, {particle, Id, P, V, A}, Acc) ->
  NewVelocity = add_vector(V, A),
  NewParticle = {particle, Id, add_vector(P, NewVelocity), NewVelocity, A},
  calculate_points(Remaining - 1, NewParticle, [P|Acc]).
