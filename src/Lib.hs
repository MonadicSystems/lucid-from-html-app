{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( someFunc,
  )
where

import Data.Text
import Lucid
import Lucid.Alpine
import Lucid.HTMX
import Lucid.Generate hiding (Html)
import Lucid.Supplemental
import Lucid.Servant
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.HTML.Lucid
import Servant.Server
import Lucid.Combinators (html5S)
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Lucid (ToHtml(toHtmlRaw))
import Lucid.Supplemental

someFunc :: IO ()
someFunc = runTLS tlsOpts warpOpts app
  where
    tlsOpts = tlsSettings "certificate.pem" "key.pem"
    warpOpts = setPort 8080 defaultSettings

data Payload = Payload
    { input :: Text
    } deriving (Generic, FromJSON)

app :: Application
app = serve (Proxy :: Proxy API) server

type API = Get '[HTML] Index :<|> ("translate" :> ReqBody '[JSON] Payload :> Post '[HTML] Text)

server :: Server API
server = returnIndex :<|> returnLucid
  where
    returnIndex :: Handler Index
    returnIndex = pure Index

    returnLucid :: Payload -> Handler Text
    returnLucid (Payload html) = do
        pure . pack $ lucidFromHtml html5S (Options False True 4) "result" $ unpack html

data Index = Index

instance ToHtml Index where
  toHtml _ = do
    doctype_

    html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        title_ "HTML -> Lucid"

        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())

        script_ [src_ "https://unpkg.com/htmx.org@latest"] ("" :: Html ())
        script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] ("" :: Html ())
        
        script_ [src_ "https://unpkg.com/hyperscript.org@0.9.3"] ("" :: Html ())

      body_ [] $ do
        h1_ [class_ "text-2xl m-10 font-semibold"] "HTML -> Lucid"
        div_
          [ class_ "m-10 flex flex-col lg:flex-row"
          ] $ do
          div_ [class_ "flex flex-col grow"] $ do
            h1_ [class_ "text-xl"] "HTML"
            textarea_
              [ class_ "mt-5 rounded-md p-2 border border-2 border-gray-500 h-screen", wrap_ "off", id_ "input", name_ "input"] "Put HTML here"
          div_ [class_ "flex flex-col my-5 lg:mx-10 self-center"] $ do
              button_
                [ class_ "text-semibold text-white bg-blue-500 rounded-md shadow-md py-2 px-4"
                , hxPost_ "/translate"
                , hxInclude_ "#input"
                , hxTarget_ "#output"
                , hxTrigger_ "click"
                , hxSwap_ "morph"
                , hxExt_ "json-enc"
                , __ "on click put 'Copy' into #copyButton"
                ]
                "Translate"
              button_
                [ class_ "text-semibold text-white bg-green-500 rounded-md shadow-md py-2 px-4 mt-5"
                , id_ "copyButton"
                , __ "on click async call navigator.clipboard.writeText(the #output's innerHTML) then put 'Copied!' into me"
                -- , __ "on click log 101"
                ]
                "Copy"
          div_ [class_ "flex flex-col grow"] $ do
            h1_ [class_ "text-xl"] "Lucid"
            textarea_
              [ class_ "mt-5 rounded-md p-2 border border-2 border-gray-500 h-screen"
              , wrap_ "off"
              , id_ "output"
              ]
              ""

  toHtmlRaw = toHtml
