%% Stop list compaction
set_prolog_flag(toplevel_print_options, [quoted(true),numbervars(true),portrayed(true),max_depth(0)]).


one_gen(F1, F2):-
  write('F1 board'), nl, draw_board(F1), nl,  
  write('F2 board'), nl, next_generation(F1, F2), draw_board(F2), nl.

third_fourth_gen(F1, F2, F3, F4):-
  one_gen(F1, F2),
  write('F3 board'), nl, next_generation(F2, F3), draw_board(F3), 
  write('F4 board'), nl, next_generation(F3, F4), draw_board(F4).




%% Question 3: test_strategy/3

% test_strategy/3: Out of N games, prints the shortest and shortest game, 
% the average game length and time, and the number of P1 vs P2 wins
test_strategy(N, StrategyP1, StrategyP2):-
  playNGames(N, StrategyP1, StrategyP2, 0, 0, 0, 0, 0, 0, 0, 0). 

% playNGames/11
% Major recursive function playing N games and acumulating game data
% Once base case is hit, data is analysed and written to prolog shell
% high arity is due to efficency, rather than carrying information
% to basecase via a list, the information is updated during recursive
% steps
playNGames(0, StrategyP1, StrategyP2, 
           P1wins, P2wins, Draws, TotalMoves, TotalTime, Games,
           ShortestGame, LongestGame):-

  Avgames is TotalMoves / Games,                  % Calc averages
  AvTime  is (TotalTime / Games) / 1000,

  % Display tournament results:
  write('Draws: '),                write(Draws),         nl,
  write('P1 wins: '),              write(P1wins),        nl,
  write('P2 wins: '),              write(P2wins),        nl,
  write('Longest game: '),         write(LongestGame),   nl,
  write('Shortest game: '),        write(ShortestGame),  nl,
  write('Average game length: '),  write(Avgames),       nl,
  write('Average game time (s): '),    write(AvTime),        nl.


playNGames(N, StrategyP1, StrategyP2, 
           P1wins, P2wins, Draws, TotalMoves, TotalTime, Games,
           ShortestGame, LongestGame):-

  % Determine time for each game
  statistics(runtime, [Tstart|_]),
  play(quiet, StrategyP1, StrategyP2, Moves, Outcome),
  statistics(runtime, [Tend|_]),
  GameTime is Tend - Tstart,

  % Decrease game counter
  NewN is N-1,

  % Update partial score sums for the last game
  update_game_stats(Outcome, P1wins, P2wins, Draws, _, NewP1, NewP2, NewDraws, _),

  % Add moves, game time and game to their respective partial sums
  sum(TotalMoves, Moves, NewTotalMoves),
  sum(TotalTime, GameTime, NewTotalTime),
  sum(Games, 1, NewGames),                % increment games by one

  % update shortest longest game
  longest_game(Outcome, LongestGame, Moves, NewLongest),
  shortest_game(ShortestGame, Moves, NewShortest),

  playNGames(NewN, StrategyP1, StrategyP2,
             NewP1, NewP2, NewDraws, 
             NewTotalMoves, NewTotalTime, NewGames, NewShortest, NewLongest).


sum(A, B, C):-
  C is A + B.

% longest_game/4 Pick the new longest game out of the current longest
% game, and the last game
longest_game(Outcome, CurrentLongest, LastGame, NewLongest):-
  Outcome \= exhaust,
  LastGame > CurrentLongest -> NewLongest = LastGame;
  NewLongest = CurrentLongest.

% discount exhausted outcome games
longest_game(Outcome, CurrentLongest, LastGame, NewLongest):-
  Outcome = exhaust,
  NewLongest = CurrentLongest.

% shortest_game/3 Pick the new shortest game out of the current shortest game,
% and the last game
shortest_game(CurrentShortest, LastGame, NewShortest):-
  % After first game, the game is both the shortest and the longest
  CurrentShortest = 0 -> NewShortest = LastGame;      % make first game shortest
  LastGame < CurrentShortest -> NewShortest = LastGame;
  NewShortest = CurrentShortest.



% update_game_stats/9: Increments Blue (P1), Red (P2), Draw and Exhaust
% counts accoring to the last game outcome
update_game_stats(Outcome, P1, P2, Draws, Exhausts,
                    NewP1, NewP2, NewDraws, NewExhausts):-
  (Outcome = b,
   NewP1 is P1+1, NewP2 = P2, NewDraws = Draws, NewExhausts = Exhausts;
   Outcome = r, 
   NewP1 = P1, NewP2 is P2+1, NewDraws = Draws, NewExhausts = Exhausts;
   Outcome = draw,
   NewP1 = P1, NewP2 = P2, NewDraws is Draws+1, NewExhausts = Exhausts;
   Outcome = stalemate,
   NewP1 = P1, NewP2 = P2, NewDraws is Draws+1, NewExhausts = Exhausts;
   Outcome = exhaust,
   NewP1 = P1, NewP2 = P2, NewDraws = Draws, NewExhausts is Exhausts+1).



% Part 2: Implementing strategies
% Question 5.

% count_pieces_after_crank/5: Takes the current board state, tries a single move
% gets the board state after the move and counts the number of pieces
% This predicate is used to search for the best move accoring to each strategy
count_pieces_after_crank(Colour, [Blues, Reds], Move, BlueCount, RedCount):-
  Colour = r,                          % simulate a red move and crank
  alter_board(Move, Reds, NewReds),
  next_generation([Blues, NewReds], CrankedAfterMove),
  count_pieces(CrankedAfterMove, BlueCount, RedCount);
  
  Colour = b,                          % simulate a blue move and crank
  alter_board(Move, Blues, NewBlues),
  next_generation([NewBlues, Reds], CrankedAfterMove),
  count_pieces(CrankedAfterMove, BlueCount, RedCount).

% count_pieces/3: Counts the number of blue and red pieces on the board,
% by determining the length of the blue and red list
count_pieces([Blues, Reds], BlueCount, RedCount):-  
  length(Blues, BlueCount), length(Reds, RedCount).


% generate_possible_moves/3 for a given colour, generate all the moves
% the colour can perform
generate_possible_moves(PieceColour, [Blues, Reds], PieceMoves):-
  PieceColour = r,        % Find all red moves
  findall([A,B,MA,MB], (member([A,B], Reds), neighbour_position(A,B, [MA, MB]),
                        \+ member([MA, MB], Reds), \+ member([MA, MB], Blues)),
          PieceMoves);

  PieceColour = b,        % Find all blue moves
  findall([A,B,MA,MB], (member([A,B], Blues), neighbour_position(A,B, [MA, MB]),
                        \+ member([MA, MB], Reds), \+ member([MA, MB], Blues)),
          PieceMoves).

  

% bloodlust/4: Strategy for a player using bloodlust is to play the move that
% minimizes the total number of opositions pieces post-crank. 
% Preservation of own peices are not takein into account

bloodlust(PlayerColour, [Blues, Reds], NewBoardState, Move):-
  generate_possible_moves(PieceColour, [Blues, Reds], MovesList),
  bloodlust_2(PlayerColour, MovesList, [Blues, Reds], 65, FirstMove, BloodMove),
  Move = BloodMove,                  % Init move to blood move
  (PlayerColour = b -> alter_board(BloodMove, Blues, NewBlues),
   NewBoardState = [NewBlues, Reds];
   alter_board(BloodMove, Reds, NewReds), NewBoardState = [Blues, NewReds]).


% bloodlust_2/6 Each move from the possible moves for a given colour is performed,
% the board is then cranked and the opposing pieces are counted.
% If the last move was bloodier than the current bloodiest, this move is set
% to the most bloodiest move

bloodlust_2(PlayerColour, [], Board, Count, Move, BloodMove):-
  BloodMove = Move. % Instantiate blood move after checking all moves

bloodlust_2(b, [X|Xs], Board, Count, CurrentBloodMove, BloodMove):-
  count_pieces_after_crank(b, Board, X, _, RedCount),
  (RedCount < Count -> NewCount = RedCount, NewBloodMove = X;                
  NewCount = Count, NewBloodMove = CurrentBloodMove),
  bloodlust_2(b, Xs, Board, NewCount, NewBloodMove, BloodMove).

bloodlust_2(r, [X|Xs], Board, Count, CurrentBloodMove, BloodMove):-
  count_pieces_after_crank(r, Board, X, BlueCount, _),
  (BlueCount < Count -> NewCount = BlueCount, NewBloodMove = X;
  NewCount = Count, NewBloodMove = CurrentBloodMove),
  bloodlust_2(r, Xs, Board, NewCount, NewBloodMove, BloodMove).



% self_preservation/4: Strategy for player using self_preservation
% is to choose the move that maximises the total number of the players own 
% pieces post-crank. Death of opponent pieces are not taken into account.

self_preservation(PlayerColour, [Blues, Reds], NewBoardState, Move):-
  generate_possible_moves(PlayerColour, [Blues, Reds], MovesList),
  self_preservation_2(PlayerColour, MovesList, [Blues, Reds], -1, FirstMove,
      SafeMove),
  Move = SafeMove,
  (PlayerColour = b -> alter_board(SafeMove, Blues, NewBlues),
   NewBoardState = [NewBlues, Reds];
   alter_board(SafeMove, Reds, NewReds), NewBoardState = [Blues, NewReds]).

self_preservation_2(PieceColour, [], Board, Count, Move, SafeMove):-
  SafeMove = Move.  % Instantiate safest move after checking all moves

self_preservation_2(b, [X|Xs], Board, Count, CurrentSafest, SafeMove):-
  count_pieces_after_crank(b, Board, X, BlueCount, _),
  (BlueCount > Count -> NewCount = BlueCount, NewSafe = X;
   NewCount = Count, NewSafe = CurrentSafest),
  self_preservation_2(b, Xs, Board, NewCount, NewSafe, SafeMove).


self_preservation_2(r, [X|Xs], Board, Count, CurrentSafest, SafeMove):-
  count_pieces_after_crank(r, Board, X, _, RedCount),
  (RedCount > Count -> NewCount = RedCount, NewSafe = X;
   NewCount = Count, NewSafe = CurrentSafest),
  self_preservation_2(r, Xs, Board, NewCount, NewSafe, SafeMove).


% land_grab/4 Strategy for player using land_grab is to maximize the function
% Player_pieces - Opponent_pieces. The result is maximizing cell occupancy

land_grab(PlayerColour, [Blues, Reds], NewBoardState, Move):-
  generate_possible_moves(PlayerColour, [Blues, Reds], MovesList),
  land_grab_2(PlayerColour, MovesList, [Blues, Reds],
              -65, FirstMove, GrabMove, _),
  Move = GrabMove,
  (PlayerColour = b -> alter_board(GrabMove, Blues, NewBlues),
   NewBoardState = [NewBlues, Reds];
   alter_board(GrabMove, Reds, NewReds), NewBoardState = [Blues, NewReds]).


land_grab_2(PlayerColour, [], Board, Count, CurrentGrab, GrabMove, GrabCount):-
  GrabMove = CurrentGrab, GrabCount = Count.

land_grab_2(b, [X|Xs], Board, Count, CurrentGrab, GrabMove, GrabCount):-
  count_pieces_after_crank(b, Board, X, BlueCount, RedCount),
  Diff is BlueCount - RedCount,
  (Diff > Count -> NewCount = Diff, NewGrab = X;
   NewCount = Count, NewGrab = CurrentGrab),
  land_grab_2(b, Xs, Board, NewCount, NewGrab, GrabMove, GrabCount).
  
land_grab_2(r, [X|Xs], Board, Count, CurrentGrab, GrabMove, GrabCount):-
  count_pieces_after_crank(r, Board, X, BlueCount, RedCount),
  Diff is RedCount - BlueCount,
  (Diff > Count -> NewCount = Diff, NewGrab = X;
   NewCount = Count, NewGrab = CurrentGrab),
  land_grab_2(r, Xs, Board, NewCount, NewGrab, GrabMove, GrabCount).


% minimax/4 Strategy for player uses a 2-ply game tree.
% The strategy tries to pick the move that constrains the opponent in such
% a way that the worst move they can play(min), is the best(max) of the possible 
% worst moves they could have played, had the player chosen a different move.
% Hence minimax.

minimax(PlayerColour, [Blues, Reds], NewBoardState, Move):-
  generate_possible_moves(PlayerColour, [Blues, Reds], MovesList),
  minmax_2(PlayerColour, MovesList, [Blues, Reds], 
            65, FirstMove, MinMaxMove),
  Move = MinMaxMove,
  (PlayerColour = b -> alter_board(MinMaxMove, Blues, NewBlues),
   NewBoardState = [NewBlues, Reds];
   alter_board(MinMaxMove, Reds, NewReds), NewBoardState = [Blues, NewReds]).



minmax_2(PlayerColour, [], Board, MinMaxCount, CurrentMM, MinMaxMove):-
  MinMaxMove = CurrentMM.

minmax_2(b, [X|Xs], [Blues, Reds], MinMaxCount, CurrentMM, MinMaxMove):-
  alter_board(X, Blues, NewBlues),    % try a new move from Max (b)
  next_generation([NewBlues, Reds], BoardAfterMaxMove),  % crank board (1-ply)
  
  % generate Mins (r) moves after Max's move on the 1-ply board
  generate_possible_moves(r, BoardAfterMaxMove, RedLeafMoves),
  % find the min move for Max - which is best land grab move for r
  land_grab_2(r, RedLeafMoves, 
			BoardAfterMaxMove, -65, FirstMove, _, LastGrabDiff),

	% if the most damaging move the oppenent can make is less damaging
	% than the current most damaging move the oponent can make,
	% then update minmax move
  (LastGrabDiff < MinMaxCount -> NewMinMaxCount = LastGrabDiff,
   NewCurrentMM = X;
   NewMinMaxCount = MinMaxCount, NewCurrentMM = CurrentMM),
  minmax_2(b, Xs, [Blues, Reds], NewMinMaxCount, NewCurrentMM, MinMaxMove).
   

minmax_2(r, [X|Xs], [Blues, Reds], MinMaxCount, CurrentMM, MinMaxMove):-
  alter_board(X, Reds, NewReds),		% try a new move from Max (b)
	write('Trying move: '), write(X), nl,
  next_generation([Blues, NewReds], BoardAfterMaxMove), % crank board 1-ply

	% generate Mins (r) after Max's move on the 1-ply board
  generate_possible_moves(b, BoardAfterMaxMove, BlueLeafMoves),
	% find the min move for Max - which is best land grab move for b
	% i.e opponent plays most damaging move possible
  land_grab_2(b, BlueLeafMoves, 
              BoardAfterMaxMove, -65, FirstMove, _, LastGrabDiff),

	% if the most damaging move the oppenent can make is less damaging
	% than the current most damaging move the oponent can make,
	% then update minmax move
  (LastGrabDiff < MinMaxCount -> NewMinMaxCount = LastGrabDiff,
	 write('Updating Minmax move: '), write(X), nl,
   NewCurrentMM = X;
   NewMinMaxCount = MinMaxCount, NewCurrentMM = CurrentMM),
  minmax_2(r, Xs, [Blues, Reds], NewMinMaxCount, NewCurrentMM, MinMaxMove).



run :-
	test_strategy(1000, random, random).
































%%  add_element(Moves, MoveList, NewMoveList),
%%  add_element(Winner, WinList, NewWinList),
%%  NewN is N - 1,
%%  playNGames(NewN, StrategyP1, StrategyP2, NewWinList, NewMoveList, TimeList).
%
%% add_element/3: adds element New to a list
%add_element(New, [], [New]).
%add_element(New, [X|Xs], [New,X|Xs]).
%   
%
%% extreme_element/3: finds the element with the minimum of maxium value
%% from a list of integer data elements. MinMax must be ground to specify whether
%% the smallest or largest datapoint should be found
%extreme_element(MinMax, [], Extreme).              % set extreme to last element
%extreme_element(MinMax, [X|Xs], 0):-
%  extreme_element(MinMax, Xs, X).                  % seed first element
%
%
%% minimum element
%extreme_element(MinMax, [X|Xs], Extreme):-
%  MinMax = minimum, (X < Extreme-> NewExtreme = X; NewExtreme = Extreme),
%  extreme_element(MinMax, Xs, NewExtreme).
%
%% maximum element
%extreme_element(MinMax, [X|Xs], Extreme):-
%  X \= 250,                                  % reject exhaustive games
%  MinMax = maximum, (X > Extreme -> NewExtreme = X; NewExtreme = Extreme),
%  extreme_element(MinMax, Xs, NewExtreme).
%
%% handle exhaust rejection by skiping element
%extreme_element(MinMax, [X|Xs], Extreme):-
%  X = 250,                        
%  MinMax = maximum,
%  extreme_element(MinMax, Xs, Extreme).
%
%average_moves([], N, TotalMoves, Av):-
%  Av is TotalMoves / N.
%average_moves([X|Xs], N, OldAcc, Av):-
%  NewAcc is OldAcc + X, average_moves(Xs, N, NewAcc, Av).
