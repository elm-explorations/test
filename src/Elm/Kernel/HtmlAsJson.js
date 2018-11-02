/*

import Elm.Kernel.Json exposing (wrap)

*/


// NOTE: this is duplicating constants currently defined in InternalTypes.elm
var virtualDomKernelConstants =
  {
    nodeTypeThunk: 5,
    kids: "e",
    refs: "l",
    thunk: "m",
    node: "k"
  }

function forceThunks(vNode) {
  // TODO: is there test coverage for this?
  // if (typeof vNode !== "undefined" && vNode.ctor === "_Tuple2" && !vNode.node) {
  //     vNode._1 = forceThunks(vNode._1);
  // }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeThunk && !vNode[virtualDomKernelConstants.node]) {
      var args = vNode[virtualDomKernelConstants.thunk];
      vNode[virtualDomKernelConstants.node] = vNode[virtualDomKernelConstants.thunk].apply(args);
  }
  // TODO: hopefully tested by Events.elm
  // if (typeof vNode !== 'undefined' && vNode.type === 'tagger') {
  //     vNode.node = forceThunks(vNode.node);
  // }
  if (typeof vNode !== 'undefined' && typeof vNode[virtualDomKernelConstants.kids] !== 'undefined') {
      vNode[virtualDomKernelConstants.kids] = vNode[virtualDomKernelConstants.kids].map(forceThunks);
  }
  return vNode;
}

function _HtmlAsJson_toJson(html)
{
  return _Json_wrap(forceThunks(html));
}

function _HtmlAsJson_eventDecoder(event)
{
  return event.decoder;
}

function _HtmlAsJson_taggerFunction(tagger)
{
  return tagger;
}

function _HtmlAsJson_attributeToJson(attribute)
{
  return _Json_wrap(attribute);
}
