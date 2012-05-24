---
title: 对Snaplet的Request local state的理解
author: Haisheng, Wu
tags: snap, snaplet
---

## 什么Snaplet

Snap从0.6版本引入Snaplet这个设计，它使得Web应用什么可组合化，开发者可以设计许多独立的功能模块，或者说小的应用模块，然后通过组合以组建一个大型的应用。

详细资料请看这里[^snaplets-tutorial]。


## Request local state

**Request Local State** 是snaplet的一个设计目标[^snaplets-design]。初识时并没引起什么关注，也是不太理解具体含义。最近在写Snaplet-OAuth的时候遇到问题，就是由于不知道这个东西的含义所造成的。


## Snaplet-oauth-0.0.0

根据Snaplet的常规模式，定义一个data type用于保存相关信息，比如

~~~
data OAuthSnaplet = OAuthSnaplet 
                    { getOauth     :: OAuth2
                    , getCodeParam :: BS.ByteString
                    } 

class HasOauth b where
    oauthLens :: Lens b (Snaplet OAuthSnaplet)

data OAuth2 = OAuth2 { oauthClientId :: BS.ByteString
                     , oauthClientSecret :: BS.ByteString
                     , oauthOAuthorizeEndpoint :: BS.ByteString
                     , oauthAccessTokenEndpoint :: BS.ByteString
                     , oauthCallback :: Maybe BS.ByteString
                     , oauthAccessToken :: Maybe BS.ByteString
                     } deriving (Show, Eq)
~~~

如果从Monad State Trans的角度去理解， `OAuthSnaplet`就是一个要成为State的一个用户类型。 
`HasOauth`可以理解为用户和其他Snaplet组合的接口。

如下代码就展示了如果将OAuthSnaplet加入到一个新的应用程序。(其实就是另一个Snaplet)
如果你已用过其他Snaplet，这看上去会很熟悉、常规。

~~~
data App = App
    { _weibo   :: Snaplet OAuthSnaplet
    }

makeLens ''App

instance HasOauth App where
   oauthLens = weibo
~~~

拿新浪微薄[^weiboapi]举例，OAuth的验证简单来说就是

1. 重定向到新浪微薄OAuth的验证页面，让用户授权
2. 授权后新浪微薄会调用我们的App指定的Callback URL
3. 我们需要实现这个Callback已获取最终的access token

下面来看下这个callback的实现

~~~~~{.haskell .numberLines}

oauthCallbackHandler :: HasOauth b 
                     => Maybe BS.ByteString
                     -> Handler b b ()
oauthCallbackHandler uri = do
    oauthSnaplet <- getOauthSnaplet
    codeParam    <- decodedParam' (getCodeParam oauthSnaplet)
    oauth        <- getOauth oauthSnaplet
    maybeToken   <- liftIO $ requestAccessToken oauth codeParam
    case maybeToken of 
        Just token -> do
             updateOAuthSnaplet (modify $ modifyOAuthState token)
             redirect $ fromMaybe "/" uri 
        _ -> writeBS "Error getting access token."


modifyOAuthState :: AccessToken -> OAuthSnaplet -> OAuthSnaplet
modifyOAuthState (AccessToken at) oa = OAuthSnaplet { getOauth = newOA, getCodeParam = getCodeParam oa }
                                       where newOA = originOA { oauthAccessToken = Just at }
                                             originOA = getOauth oa

updateOAuthSnaplet :: (MonadSnaplet m) => m b OAuthSnaplet a -> m b OAuthSnaplet a
updateOAuthSnaplet = with' oauthLens

~~~~~


这里主要关注的是第11行到13行，`Just token`表示成功获取了AccessToken，然后要

1. 将OAuthSnpalet里的oauth的AccessToken更新掉。
2. 然后将更新后OAuthSnaplet替代掉原来的

这样一来OAuthSnaplet就有AccessToken，在往后的Handler都可以拿到这个AcceeToken来访问微薄资源。

然后事实并不是这样子，在这个oauthCallbackHandler对OAuthSnaplet的更新只限于这个Handler。
**因为snap是多线程的且线程安全，每一次的request都是对snaplet状态的一份新拷贝。**
而由于初始化OauthSnaplet的时候是没有AccessToken的，这就意谓着所有的Handler默认读到的AccessToken是空的。


## 如何解决

解决方案就是把`OAuthSnaplet`里的`oauth`变成一个共享变量，这样可以在多线程之间共享。

一种实现方式就是用`MVar`[^mvar]，这样OAuthSnaplet就成了这样子


~~~
data OAuthSnaplet = OAuthSnaplet 
                    { getOauth     :: MVar OAuth2
                    , getCodeParam :: BS.ByteString
                    } 
~~~

然后就很直观了，用`Control.Concurrent`库里提供的更新一个MVar的方式来做更新和读取。
最后的实现可以参考这里[^github-snaplet-oauth]，不在这里累赘。


## 还有什么问题

你可能已经发现，这样的实现方式，如何支持多用户，以及多个OAuth Provider呢？
我还没有答案，如果你知道怎么做，欢迎[send Pull Request].

[send Pull Request]: https://github.com/HaskellCNOrg/snaplet-oauth

[^snaplets-tutorial]: [Snaplets Tutorial](http://snapframework.com/docs/tutorials/snaplets-tutorial)
[^weiboapi]: [新浪微博授权机制说明](http://open.weibo.com/wiki/%E6%8E%88%E6%9D%83%E6%9C%BA%E5%88%B6%E8%AF%B4%E6%98%8E)
[^mvar]: [MVar](http://www.haskell.org/ghc/docs/7.0-latest/html/libraries/base-4.3.1.0/Control-Concurrent-MVar.html)
[^snaplets-design]: [Snaplets-Design](http://snapframework.com/docs/tutorials/snaplets-design)
[^github-snaplet-oauth]: [Snaplet-OAuth in Github](https://github.com/HaskellCNOrg/snaplet-oauth)
