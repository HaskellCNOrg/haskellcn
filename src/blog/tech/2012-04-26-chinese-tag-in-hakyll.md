---
title: 在Hakyll中使用中文标签
author: Haisheng, Wu
tags: hakyll,encoding
---

**此问题发生在GHC < 7.2.x的环境，据说7.2.x已经修正了造成此原因的bug(未验证)。**

## 问题

Hakyll[^hakyll]会给每个标签单独生成一个html，比如*foo.html*, 用来显示标签对应的所有文章。
然而当标签为中文时，文件名会是乱码，而非对应的中文标签名。

## 原因

造成问题是原因由于Haskell里写文件的方法。 如下所是，`writeFile`用base库里的`String`类型表示文件名，
这就限制其只能表示ASC-II字符集。

~~~~~~{.haskell}
type FilePath = String
              
writeFile :: FilePath  -- ^ 文件名
          -> String    -- ^ 要写到文件的内容
          -> IO ()
 
~~~~~~

## 一种解决办法

最直接的方式就是用类库utf8-string[^utf8-string]里的`encodeString`方法，现将文件名进行encode然后再调`writeFile`方法。比如

~~~~~~{.haskell}
import qualified Codec.Binary.UTF8.String as UTF8

main = myWriteFile "新天地.html" "test"
myWriteFile = writeFile . UTF8.encodeString

~~~~~~

对应到Hakyll, 就是将上述的改动加到这个文件里*src/Hakyll/Core/Writable.hs*.详见这里[^private-3.2.6.1].

## 再一种解决办法

hakyll goole groups有人帖了他的一个解决方案，也是用utf8-string先encode，但修改的地方不一样。详见此[^another-fix]。

[^hakyll]: [Hakyll Home](http://jaspervdj.be/hakyll/)
[^private-3.2.6.1]: [Private branch for hakyll](https://github.com/freizl/hakyll/commits/private/3.2.6.1)
[^utf8-string]: [Hackage utf8-string](http://hackage.haskell.org/package/utf8-string)
[^another-fix]: [Another fix](https://github.com/hwa/hakyll/commit/cb7dc75d40fcb2ccbbea9ee3a5582d1ee7fd3cc9)

