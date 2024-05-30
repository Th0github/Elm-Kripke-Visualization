module Error exposing (KMError(..), errorToString)
import Http exposing (..)

type KMError
    = InvalidInput
    | WorldExists
    | AgentExists
    | AgentDoesNotExist
    | PropositionExists
    | RelationExists
    | InvalidRelationInput
    | NetworkError Http.Error
    | OtherError String

errorToString : KMError -> String
errorToString error =
    case error of
        AgentExists  ->
            "Error: Agent already exists"
        AgentDoesNotExist ->
            "Error: Agent does not exist"
        InvalidRelationInput ->
            "Error: Invalid relation input,"

        InvalidInput ->
            "Error: Invalid input,"

        WorldExists ->
            "Error: World already exists "

        PropositionExists ->
            "Error: Proposition already exists, "

        RelationExists ->
            "Error: Relation already exists, "

        NetworkError httpError ->
            "Error: Network issue - " ++ httpErrorToString httpError

        OtherError msg ->
            "Error: " ++ msg


httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus response ->
            "Bad status: " ++ String.fromInt response

        Http.BadBody body ->
            "Bad body: " ++ body
