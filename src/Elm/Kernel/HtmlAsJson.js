/*

import Elm.Kernel.Json exposing (wrap)

*/

function forceThunks(vNode) {
  if (typeof vNode !== "undefined" && vNode.ctor === "_Tuple2" && !vNode.node) {
      vNode._1 = forceThunks(vNode._1);
  }
  if (typeof vNode !== 'undefined' && vNode.type === 'thunk' && !vNode.node) {
      vNode.node = vNode.thunk.apply(vNode.thunk, vNode.args);
  }
  if (typeof vNode !== 'undefined' && vNode.type === 'tagger') {
      vNode.node = forceThunks(vNode.node);
  }
  if (typeof vNode !== 'undefined' && typeof vNode.children !== 'undefined') {
      vNode.children = vNode.children.map(forceThunks);
  }
  return vNode;
}

function _HtmlAsJson_toJson(html)
{
  return forceThunks(html);
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
