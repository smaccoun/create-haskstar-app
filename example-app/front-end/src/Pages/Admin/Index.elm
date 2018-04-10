module Pages.Admin.Index exposing (..)

import Server.Config


type AdminPage
    = AdminHome


type AdminRoute
    = AdminHomeRoute


initializePageFromRoute : Server.Config.Context -> AdminRoute -> AdminPage
initializePageFromRoute serverContext route =
    case route of
        AdminHomeRoute ->
            AdminHome
