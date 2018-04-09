module Pages.Index exposing (..)

import Form exposing (Form)
import Views.LoginPanel as LoginPanel


type AppPage
    = LoginPage (Form () LoginPanel.LoginForm)
