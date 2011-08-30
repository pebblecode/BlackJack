module BlackJack

type Suit = 
    | Club
    | Diamond
    | Heart
    | Spade

type Rank = 
    | Ace
    | King
    | Queen
    | Jack
    | Value of int

type Card = Card of Rank * Suit


let DeckOfCards = 
    [ 
        for s in [Club; Diamond; Heart; Spade] do
            for r in [Ace; King; Queen; Jack] do
                yield Card(r,s)
            for v in 2..10 do
                yield Card(Value v, s) 
    ]
let shuffle cards = 
    let rand = new System.Random()
    cards 
        |> List.map (fun c -> (rand.Next(), c))
        |> List.sortBy fst
        |> List.map snd

let ShuffledCards = shuffle DeckOfCards

type Player(name: string) = 
    let Name: string = name;
    let mutable Cards: Card list = []
    let CardValue (Card(r,s)) = 
        match r with
            | Ace -> (1,10)
            | King | Queen | Jack -> (10,10)
            | Value v -> (v,v)

    member p.HandTotal = 
        let GetTotal (a: int list) (x: int*int) =
            let AddVal (v: int) = List.map (fun lv -> lv + v)
            List.append 
                (a |> AddVal (fst x))
                (a |> AddVal (snd x))
        Cards 
        |> List.map CardValue 
        |> List.fold GetTotal [0] 
        |> Set.ofList

    member p.HandTotalMinimum = p.HandTotal |> Set.minElement
    member p.TakeCard(card: Card) =
        Cards <- card :: Cards
    override p.ToString() = 
        sprintf "\nPlayer %s\nCards: %A\nTotal: %A" Name Cards p.HandTotal

let playWithThree (cards: Card list) = 
    let rand = new System.Random()
    let RandomCard = 
        fun () -> cards.[rand.Next(0, cards.Length-1)]
    let players : Player list = [Player("Player-1");Player("Player-2");Player("Player-3");]
    let rec Play (p: Player) =        
        match p with
                | _ when p.HandTotalMinimum < 16 ->
                        p.TakeCard(RandomCard())
                        Play p
                | _ -> p
    players 
    |> List.map Play
    |> List.map (fun p -> printfn "%O" p)

playWithThree DeckOfCards