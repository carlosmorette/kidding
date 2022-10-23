-module(kidding).

-export([run/1, expect/2, expect_and_get/2]).

-define(LowerCaseCharList, lists.seq($a, $z)).

run(Path) ->
  case file:read_file(Path) of
    {ok, Content} ->
      {Tokens, _Line} = tokenize(binary_to_list(Content), [], 0),
      RParser = parse(Tokens),
      eval(RParser);
    {error, enoent} ->
      {error, not_found}
  end.
tokenize([], Tokens, Line) ->
  {lists:reverse(Tokens) ++ [eof], Line};
tokenize([$(|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(left_paren)|Acc], Line);
tokenize([$)|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(right_paren)|Acc], Line);
tokenize([$+|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(plus)|Acc], Line);
tokenize([$-|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(minus)|Acc], Line);
tokenize([$*|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(star)|Acc], Line);
tokenize([$/|RestTokens], Acc, Line) ->
  tokenize(RestTokens, [new_token(slash)|Acc], Line);
tokenize([$\s|RestTokens], Acc, Line)->
  tokenize(RestTokens, Acc, Line);
tokenize([$\n|RestTokens], Acc, Line)->
  tokenize(RestTokens, Acc, Line + 1);
tokenize([Char|RestTokens], Acc, Line) when (Char >= $a) andalso (Char =< $z) ->
  {NewToken, Rest} = identifier(RestTokens, [Char]),
  tokenize(Rest, [NewToken|Acc], Line);
tokenize([Char|RestTokens], Acc, Line) when (Char >= $0) andalso (Char =< $9) ->
  {NewToken, Rest} = number(RestTokens, [Char]),
  tokenize(Rest, [NewToken|Acc], Line).
identifier([Char|RestTokens], Acc) when (Char >= $a) andalso (Char =< $z) ->
  identifier(RestTokens, Acc ++ [Char]);
identifier(RestTokens, Acc) ->
  {new_token(identifier, Acc), RestTokens}.
number([Char|RestTokens], Acc) when (Char >= $0) andalso (Char =< $9) ->
  number(RestTokens, Acc ++ [Char]);
number(RestTokens, Acc) ->
  {new_token(number, Acc), RestTokens}.

new_token(Type, Value) ->
  #{type => Type, value => Value}.
new_token(Type) ->
  #{type => Type}.

parse(Tokens) ->
 parse_program(Tokens).

parse_program(Tokens) ->
  parse_program(Tokens, []).

parse_program([eof], Acc) ->
  Acc;
parse_program(Tokens, Acc) ->
  {Expr, Tokens2} = parse_expression(Tokens),
  parse_program(Tokens2, Acc ++ [Expr]);
parse_program([H|_Rest], _Acc) ->
  throw(io:format("Wrong program, expected '(', got: ~w", [H])).

parse_expression([#{type := left_paren}|_Rest]=Tokens) ->
  RTokens = expect(Tokens, left_paren),
  {ExprCaller, RTokens2} = get_current(RTokens),
  {Params, RTokens3} = parse_expr_params(RTokens2, []),
  RTokens4 = expect(RTokens3, right_paren),
  {#{
     type => expr, 
     caller => ExprCaller, 
     params => Params}, 
   RTokens4}.

parse_expr_params(Tokens, Acc) ->
  Conditions = 
    [
     {
      fun(Rt) -> 
	  match(Rt, identifier) 
      end, 
      fun(Rt) -> 
	  expect_and_get(Rt, identifier)
      end},
     {
      fun(Rt) -> 
	  match(Rt, number) 
      end, 
      fun(Rt) -> 
	  expect_and_get(Rt, number)
      end
     },
     {
      fun(Rt) ->
	  match(Rt, right_paren)
      end,
      fun(Rt) ->
	  {right_paren, Rt}
      end
     }
    ],
    case parse_conditions(Tokens, Conditions) of
      {right_paren, RTokens} ->
	{Acc, RTokens};

      {TValue, Tokens2} -> 
	parse_expr_params(Tokens2, Acc ++ [TValue])
    end.

parse_conditions(_Tokens, []) -> 
  throw("Expected identifier or number");
parse_conditions(Tokens, [{CondFun, ResFun}|Tail]) ->
  case CondFun(Tokens) of
    true ->
      ResFun(Tokens);
    false ->
      parse_conditions(Tokens, Tail)
  end.


expect([#{type := Type}|Tokens], Type) ->
  Tokens;

expect(_Tokens, Type) ->
  throw(io:format("Expected: ~w", [Type])).

get_current([H|T]) ->
  {H, T}.

expect_and_get([#{type := Type, value := Value}|Rest], Type) ->
  {Value, Rest};
expect_and_get(_Tokens, Type) ->
  throw(io:format("Expected: ~w", [Type])).

match([#{type := Type}|_], Type) ->
  true;
match(_Tokens, _Type) ->
  false.

eval([]) ->
  ok;

eval([#{caller := #{type := plus}, params := Params} | Rest]) ->
  print(sum(string_to_integer(Params))),
  eval(Rest);

eval([#{caller := #{type := minus}, params := Params} | Rest]) ->
  print(sum(string_to_integer(Params))),
  eval(Rest).

sum(ListOfNumbers) ->
  lists:foldl(fun(N, Acc) -> N + Acc end, 0, ListOfNumbers).

sub([H|ListOfNumbers]) ->
  lists:foldl(fun(N, Acc) -> N - Acc end, H, ListOfNumbers).

string_to_integer(Nums) ->
  lists:map(
    fun({N, _}) -> N end,
    lists:map(
      fun(P) -> 
	  string:to_integer(P) 
      end, Nums)).

print(X) ->
  io:write(X),
  io:fwrite("\n").

