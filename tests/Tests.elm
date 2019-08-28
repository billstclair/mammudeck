module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Mammudeck.EncodeDecode as MED
import Mammudeck.Types as Types
    exposing
        ( Feed
        , FeedElements(..)
        , FeedSet
        , FeedSetDefinition
        , FeedType(..)
        , PublicFeedFlags
        , UserFeedFlags
        )
import Mastodon.Entity exposing (NotificationType(..))
import Maybe exposing (withDefault)
import Set exposing (Set)
import Test exposing (..)


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map Debug.toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap fsdTest fsdData
            ]


expectResult : Result JD.Error thing -> Result JD.Error thing -> Expectation
expectResult sb was =
    case was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (JD.errorToString msg) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


fsdTest : FeedSetDefinition -> String -> Test
fsdTest feedSetDefinition name =
    test ("feedSetDefinition \"" ++ name ++ "\"")
        (\_ ->
            let
                value =
                    MED.encodeFeedSetDefinition feedSetDefinition

                result =
                    case JD.decodeValue MED.feedSetDefinitionDecoder value of
                        Err e ->
                            Err e

                        Ok en ->
                            Ok en
            in
            expectResult (Ok feedSetDefinition) result
        )


fsdData : List FeedSetDefinition
fsdData =
    [ { name = "fsd1"
      , feedTypes =
            [ HomeFeed, ConversationsFeed ]
      }
    , { name = "fsd2"
      , feedTypes =
            [ UserFeed
                { username = "un1"
                , server = "mastodon.social"
                , flags = Nothing
                }
            , UserFeed
                { username = "un2"
                , server = "kiwifarms.cc"
                , flags = Just uff1
                }
            , UserFeed
                { username = "un3"
                , server = "gab.com"
                , flags = Just uff2
                }
            ]
      }
    , { name = "fsd3"
      , feedTypes =
            [ PublicFeed { flags = Nothing }
            , PublicFeed { flags = Just pff1 }
            , PublicFeed { flags = Just pff2 }
            ]
      }
    , { name = "fsd4"
      , feedTypes =
            [ HashtagFeed "hash", ListFeed "list", GroupFeed "group" ]
      }
    , { name = "fsd5"
      , feedTypes =
            [ NotificationFeed
                { accountId = Nothing
                , exclusions = []
                }
            , NotificationFeed
                { accountId = Just "id2"
                , exclusions =
                    [ FollowNotification
                    , MentionNotification
                    , ReblogNotification
                    , FavouriteNotification
                    , PollNotification
                    ]
                }
            ]
      }
    , { name = "fsd6"
      , feedTypes =
            [ SearchFeed
                { q = "search"
                , resolve = True
                , following = True
                }
            , SearchFeed
                { q = "search some more"
                , resolve = True
                , following = True
                }
            , SearchFeed
                { q = "return of the son of search"
                , resolve = False
                , following = True
                }
            ]
      }
    ]


pff1 : PublicFeedFlags
pff1 =
    { local = True
    , only_media = False
    }


pff2 : PublicFeedFlags
pff2 =
    { local = False
    , only_media = True
    }


uff1 : UserFeedFlags
uff1 =
    { only_media = False
    , pinned = False
    , replies = False
    , reblogs = False
    }


uff2 : UserFeedFlags
uff2 =
    { only_media = True
    , pinned = True
    , replies = True
    , reblogs = True
    }
