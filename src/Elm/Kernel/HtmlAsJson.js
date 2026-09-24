/*

import Elm.Kernel.Json exposing (wrap)

*/


// NOTE: this is duplicating constants also defined in Test.Internal.KernelConstants
//       so if you make any changes here, be sure to synchronize them there!
var virtualDomKernelConstants =
  {
    nodeTypeCustom: 3,
    nodeTypeTagger: 4,
    nodeTypeThunk: 5,
    kids: "e",
    refs: "l",
    thunk: "m",
    node: "k",
    value: "a"
  }

// A special key the Elm side will decode to learn which custom node it's looking at.
// See the `VirtualDom.custom` comment below.
var customNodeFunctionNames = "customNodeFunctionNames";

function forceThunks(vNode) {
  if (typeof vNode !== "undefined" && vNode.$ === "#2") {
    // This is a tuple (the kids : List (String, Html) field of a Keyed node); recurse into the right side of the tuple
    vNode.b = forceThunks(vNode.b);
  }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeThunk && !vNode[virtualDomKernelConstants.node]) {
    // This is a lazy node; evaluate it
    var args = vNode[virtualDomKernelConstants.thunk];
    vNode[virtualDomKernelConstants.node] = vNode[virtualDomKernelConstants.thunk].apply(args);
    // And then recurse into the evaluated node
    vNode[virtualDomKernelConstants.node] = forceThunks(vNode[virtualDomKernelConstants.node]);
  }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeTagger) {
    // This is an Html.map; recurse into the node it is wrapping
    vNode[virtualDomKernelConstants.node] = forceThunks(vNode[virtualDomKernelConstants.node]);
  }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeCustom) {
    /*
    This is a `VirtualDom.custom` node (e.g. from elm-explorations/markdown or
    elm-explorations/webgl).

    We can identify these by their `render` and `diff` function arguments -
    this is what `vNode` looks like:

    // Markdown
    {
      '$': 3,
      d: {},
      g: {
        a: [Object],
        b: 'Some **Markdown**'
      },
      h: [Function: _Markdown_render],
      i: [Function: _Markdown_diff]
    }

    // WebGL
    {
      '$': 3,
      d: {},
      g: { g: [Object], f: {}, h: [Object] },
      h: [Function: _WebGL_render],
      i: [Function: _WebGL_diff]
    }

    We can massage these to ['_WebGL_render', '_WebGL_diff'] and fingerprint the
    custom Virtual DOM nodes that way.

    In this kernel function we put them in a special key so that the Elm-side
    JSON decoder can reach them.
    */
    vNode[customNodeFunctionNames] = Object.keys(vNode)
      .map((key) => vNode[key])
      .filter((value) => typeof value === 'function')
      .map((fn) => fn.name);
  }
  if (typeof vNode !== 'undefined' && typeof vNode[virtualDomKernelConstants.kids] !== 'undefined') {
    // This is something with children (either a node with kids : List Html, or keyed with kids : List (String, Html));
    // recurse into the children
    vNode[virtualDomKernelConstants.kids] = vNode[virtualDomKernelConstants.kids].map(forceThunks);
  }
  return vNode;
}

function _HtmlAsJson_toJson(html)
{
  return _Json_wrap(forceThunks(html));
}

function _HtmlAsJson_eventHandler(event)
{
  return event[virtualDomKernelConstants.value];
}

function _HtmlAsJson_taggerFunction(tagger)
{
  return tagger.a;
}

function _HtmlAsJson_attributeToJson(attribute)
{
  return _Json_wrap(attribute);
}
