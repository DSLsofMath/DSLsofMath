
%
% TODO by (DaHe): In general, these exercises feel like they would belong more
% to week 2 than week 3. Shouldn't some of them be moved? There is pretty much
% noting in here that has to do with this week's topics.
%


% TODO (DaHe): Exercise introducing deep vs shallow embedding using type classes
% also, maybe introduce the concept of using type classes to return a deep
% embedding by using the operators of the data type, and casting to the
% syntactic type
%
% ---------------------------------------------------------------------------
% IDEA: Use the card/hand datatypes from the blackjack haskell lab, and
% implement a deep and shallow embedding for hands (the shallow one just returns
% the sum of the cards, based on some score :: Card -> Iteger). Then show that
% when we introduce a new rule into the game, where the combination (syntax) of
% the cards in hand matters, the shallow embedding is not enough.
%
% This is the data type I have in mind:
%
% data Card = Card {rank :: Rank, suit :: Suit}
%             deriving(Eq, Show)

% data Suit = Hearts | Spades | Diamonds | Clubs
%     deriving(Eq, Show)

% data Rank = Numeric Integer | Jack | Queen | King | Ace
%     deriving(Eq, Show)

% data Hand = Empty | Add Card Hand
%     deriving(Eq, Show)
%
% I believe this is given (at least it was during my year) at the start of a
% lab, so it shouldn't spoil any solution. Alternatively, we could just use
% a different but similar data type.
% ---------------------------------------------------------------------------


% TODO (DaHe): Exercise asking to implement a deep and a shallow embedding of some data
% type, using knowlege acquired from above

% TODO (DaHe): Describe a thing, ask to implement DSL for that thing by introducing
% data type, deep shallow embedding, evaluator


%% TODO (DaHe): One or two exercieses presenting the definition of a
% mathematical concept / equation, and then ask student to type the variables and
% expressions involved. Should be split into parts, similar to exam questions.
% (This should probably come first)


