module Test.Html.Internal.ElmHtml.WebGL exposing (isWebGLCustomNode)


isWebGLCustomNode : { customNodeFunctionNames : List String } -> Bool
isWebGLCustomNode { customNodeFunctionNames } =
    List.any isWebGLKernelFunctionName customNodeFunctionNames


isWebGLKernelFunctionName : String -> Bool
isWebGLKernelFunctionName name =
    (name == "_WebGL_render")
        || (name == "_WebGL_diff")
