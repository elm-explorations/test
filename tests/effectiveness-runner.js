(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Test_runThunk(thunk)
{
  try {
    // Attempt to run the thunk as normal.
    return $elm$core$Result$Ok(thunk(_Utils_Tuple0));
  } catch (err) {
    // If it throws, return an error instead of crashing.
    return $elm$core$Result$Err(err.toString());
  }
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = {$: 'GT'};
var $author$project$EffectivenessMain$RunConfig = F5(
	function (totalSeeds, multiplier, addend, targetFailures, maxFuzz) {
		return {addend: addend, maxFuzz: maxFuzz, multiplier: multiplier, targetFailures: targetFailures, totalSeeds: totalSeeds};
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $author$project$EffectivenessMain$defaultConfig = {addend: 0, maxFuzz: 1000000, multiplier: 1, targetFailures: 10, totalSeeds: 1000};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$EffectivenessMain$decodeFlags = A6(
	$elm$json$Json$Decode$map5,
	$author$project$EffectivenessMain$RunConfig,
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$Maybe$withDefault($author$project$EffectivenessMain$defaultConfig.totalSeeds),
		$elm$json$Json$Decode$maybe(
			A2($elm$json$Json$Decode$field, 'totalSeeds', $elm$json$Json$Decode$int))),
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$Maybe$withDefault($author$project$EffectivenessMain$defaultConfig.multiplier),
		$elm$json$Json$Decode$maybe(
			A2($elm$json$Json$Decode$field, 'multiplier', $elm$json$Json$Decode$int))),
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$Maybe$withDefault($author$project$EffectivenessMain$defaultConfig.addend),
		$elm$json$Json$Decode$maybe(
			A2($elm$json$Json$Decode$field, 'addend', $elm$json$Json$Decode$int))),
	$elm$json$Json$Decode$succeed($author$project$EffectivenessMain$defaultConfig.targetFailures),
	$elm$json$Json$Decode$succeed($author$project$EffectivenessMain$defaultConfig.maxFuzz));
var $author$project$EffectivenessMain$decodeFlagsWithDefaults = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			$author$project$EffectivenessMain$decodeFlags,
			$elm$json$Json$Decode$succeed($author$project$EffectivenessMain$defaultConfig)
		]));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$Debug$log = _Debug_log;
var $elm$core$String$length = _String_length;
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$EffectivenessSUT$BstCommon$Leaf = {$: 'Leaf'};
var $author$project$EffectivenessSUT$BstCommon$Node = F4(
	function (a, b, c, d) {
		return {$: 'Node', a: a, b: b, c: c, d: d};
	});
var $author$project$EffectivenessSUT$Bst1$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst1$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst1$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst1$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst1$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst1$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst1$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst1$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst2$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst2$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst2$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst2$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst2$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst2$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst2$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst2$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst3$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst3$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst3$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst3$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst3$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst3$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst3$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst3$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst4$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst4$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst4$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst4$delete = F2(
	function (k, tree) {
		_delete:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 0) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue _delete;
				} else {
					if (_Utils_cmp(k, key) > 0) {
						var $temp$k = k,
							$temp$tree = right;
						k = $temp$k;
						tree = $temp$tree;
						continue _delete;
					} else {
						var _v1 = _Utils_Tuple2(left, right);
						if (_v1.a.$ === 'Leaf') {
							var _v2 = _v1.a;
							return right;
						} else {
							if (_v1.b.$ === 'Leaf') {
								var _v3 = _v1.b;
								return left;
							} else {
								var _v4 = $author$project$EffectivenessSUT$Bst4$minNode(right);
								if (_v4.$ === 'Nothing') {
									return tree;
								} else {
									var _v5 = _v4.a;
									var succKey = _v5.a;
									var succVal = _v5.b;
									var newRight = $author$project$EffectivenessSUT$Bst4$deleteMin(right);
									return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
								}
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst5$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst5$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst5$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst5$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					left,
					key,
					val,
					A2($author$project$EffectivenessSUT$Bst5$delete, k, right));
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst5$delete, k, left),
						key,
						val,
						right);
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst5$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst5$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst6$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst6$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst6$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst6$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst6$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst6$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst6$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst6$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst7$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst7$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst7$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst7$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst7$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst7$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst7$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst7$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst8$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst8$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst8$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst8$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst8$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst8$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst8$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst8$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $elm_explorations$test$Test$Runner$Failure$BadDescription = {$: 'BadDescription'};
var $elm_explorations$test$Test$Runner$Failure$DuplicatedName = {$: 'DuplicatedName'};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Batch = function (a) {
	return {$: 'ElmTestVariant__Batch', a: a};
};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Labeled = F2(
	function (a, b) {
		return {$: 'ElmTestVariant__Labeled', a: a, b: b};
	});
var $elm_explorations$test$Test$Runner$Failure$EmptyList = {$: 'EmptyList'};
var $elm_explorations$test$Test$Runner$Failure$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$isEmpty(dict);
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_explorations$test$Test$Internal$duplicatedName = function (tests) {
	var names = function (test) {
		names:
		while (true) {
			switch (test.$) {
				case 'ElmTestVariant__Labeled':
					var str = test.a;
					return _List_fromArray(
						[str]);
				case 'ElmTestVariant__Batch':
					var subtests = test.a;
					return A2($elm$core$List$concatMap, names, subtests);
				case 'ElmTestVariant__UnitTest':
					return _List_Nil;
				case 'ElmTestVariant__FuzzTest':
					return _List_Nil;
				case 'ElmTestVariant__Skipped':
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
				default:
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
			}
		}
	};
	var accumDuplicates = F2(
		function (newName, _v2) {
			var dups = _v2.a;
			var uniques = _v2.b;
			return A2($elm$core$Set$member, newName, uniques) ? _Utils_Tuple2(
				A2($elm$core$Set$insert, newName, dups),
				uniques) : _Utils_Tuple2(
				dups,
				A2($elm$core$Set$insert, newName, uniques));
		});
	var _v1 = A3(
		$elm$core$List$foldl,
		accumDuplicates,
		_Utils_Tuple2($elm$core$Set$empty, $elm$core$Set$empty),
		A2($elm$core$List$concatMap, names, tests));
	var dupsAccum = _v1.a;
	var uniquesAccum = _v1.b;
	return $elm$core$Set$isEmpty(dupsAccum) ? $elm$core$Result$Ok(uniquesAccum) : $elm$core$Result$Err(dupsAccum);
};
var $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest = function (a) {
	return {$: 'ElmTestVariant__UnitTest', a: a};
};
var $elm_explorations$test$Test$Expectation$Fail = function (a) {
	return {$: 'Fail', a: a};
};
var $elm_explorations$test$Test$Distribution$NoDistribution = {$: 'NoDistribution'};
var $elm_explorations$test$Test$Expectation$fail = function (_v0) {
	var description = _v0.description;
	var reason = _v0.reason;
	return $elm_explorations$test$Test$Expectation$Fail(
		{description: description, distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution, given: $elm$core$Maybe$Nothing, reason: reason});
};
var $elm_explorations$test$Test$Internal$failNow = function (record) {
	return $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest(
		function (_v0) {
			return _List_fromArray(
				[
					$elm_explorations$test$Test$Expectation$fail(record)
				]);
		});
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$trim = _String_trim;
var $elm_explorations$test$Test$describe = F2(
	function (untrimmedDesc, tests) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		if ($elm$core$String$isEmpty(desc)) {
			return $elm_explorations$test$Test$Internal$failNow(
				{
					description: 'This `describe` has a blank description. Let\'s give it a useful one!',
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
				});
		} else {
			if ($elm$core$List$isEmpty(tests)) {
				return $elm_explorations$test$Test$Internal$failNow(
					{
						description: 'This `describe ' + (desc + '` has no tests in it. Let\'s give it some!'),
						reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
					});
			} else {
				var _v0 = $elm_explorations$test$Test$Internal$duplicatedName(tests);
				if (_v0.$ === 'Err') {
					var dups = _v0.a;
					var dupDescription = function (duped) {
						return 'Contains multiple tests named \'' + (duped + '\'. Let\'s rename them so we know which is which.');
					};
					return A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: A2(
									$elm$core$String$join,
									'\n',
									A2(
										$elm$core$List$map,
										dupDescription,
										$elm$core$Set$toList(dups))),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							}));
				} else {
					var childrenNames = _v0.a;
					return A2($elm$core$Set$member, desc, childrenNames) ? A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: 'The test \'' + (desc + '\' contains a child test of the same name. Let\'s rename them so we know which is which.'),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							})) : A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$ElmTestVariant__Batch(tests));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst1$insert = F3(
	function (k, v, _v0) {
		return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
	});
var $author$project$EffectivenessSUT$Bst2$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst2$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst2$insert, k, v, right)) : A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst2$insert, k, v, left),
				key,
				val,
				right));
		}
	});
var $author$project$EffectivenessSUT$Bst3$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst3$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst3$insert, k, v, right)) : tree);
		}
	});
var $author$project$EffectivenessSUT$Bst4$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst4$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst4$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst5$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst5$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst5$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst6$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst6$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst6$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst7$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst7$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst7$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst8$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst8$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst8$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst$insert = F3(
	function (k, v, tree) {
		if (tree.$ === 'Leaf') {
			return A4($author$project$EffectivenessSUT$BstCommon$Node, $author$project$EffectivenessSUT$BstCommon$Leaf, k, v, $author$project$EffectivenessSUT$BstCommon$Leaf);
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			return (_Utils_cmp(k, key) < 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				A3($author$project$EffectivenessSUT$Bst$insert, k, v, left),
				key,
				val,
				right) : ((_Utils_cmp(k, key) > 0) ? A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				left,
				key,
				val,
				A3($author$project$EffectivenessSUT$Bst$insert, k, v, right)) : A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, right));
		}
	});
var $author$project$EffectivenessSUT$Bst$fromList = function (pairs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, acc) {
				var k = _v0.a;
				var v = _v0.b;
				return A3($author$project$EffectivenessSUT$Bst$insert, k, v, acc);
			}),
		$author$project$EffectivenessSUT$BstCommon$Leaf,
		pairs);
};
var $elm_explorations$test$Fuzz$Internal$Fuzzer = function (a) {
	return {$: 'Fuzzer', a: a};
};
var $elm_explorations$test$GenResult$Rejected = function (a) {
	return {$: 'Rejected', a: a};
};
var $elm_explorations$test$Fuzz$andThen = F2(
	function (fn, _v0) {
		var fuzzer = _v0.a;
		return $elm_explorations$test$Fuzz$Internal$Fuzzer(
			function (prng) {
				var _v1 = fuzzer(prng);
				if (_v1.$ === 'Generated') {
					var g = _v1.a;
					var _v2 = fn(g.value);
					var newFuzzer = _v2.a;
					return newFuzzer(g.prng);
				} else {
					var r = _v1.a;
					return $elm_explorations$test$GenResult$Rejected(r);
				}
			});
	});
var $elm_explorations$test$GenResult$Generated = function (a) {
	return {$: 'Generated', a: a};
};
var $elm_explorations$test$Fuzz$constant = function (x) {
	return $elm_explorations$test$Fuzz$Internal$Fuzzer(
		function (prng) {
			return $elm_explorations$test$GenResult$Generated(
				{prng: prng, value: x});
		});
};
var $elm_explorations$test$PRNG$Hardcoded = function (a) {
	return {$: 'Hardcoded', a: a};
};
var $elm_explorations$test$PRNG$Random = function (a) {
	return {$: 'Random', a: a};
};
var $elm_explorations$test$Queue$Queue = F2(
	function (a, b) {
		return {$: 'Queue', a: a, b: b};
	});
var $elm_explorations$test$Queue$queue = F2(
	function (fl, rl) {
		if (!fl.b) {
			return A2(
				$elm_explorations$test$Queue$Queue,
				$elm$core$List$reverse(rl),
				_List_Nil);
		} else {
			return A2($elm_explorations$test$Queue$Queue, fl, rl);
		}
	});
var $elm_explorations$test$Queue$enqueue = F2(
	function (a, _v0) {
		var fl = _v0.a;
		var rl = _v0.b;
		return A2(
			$elm_explorations$test$Queue$queue,
			fl,
			A2($elm$core$List$cons, a, rl));
	});
var $elm_explorations$test$RandomRun$append = F2(
	function (n, run) {
		return _Utils_update(
			run,
			{
				data: A2(
					$elm_explorations$test$Queue$enqueue,
					A2($elm$core$Basics$max, 0, n),
					run.data),
				length: run.length + 1
			});
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm_explorations$test$Queue$dequeue = function (_v0) {
	var fl = _v0.a;
	var rl = _v0.b;
	if (!fl.b) {
		return _Utils_Tuple2(
			$elm$core$Maybe$Nothing,
			A2($elm_explorations$test$Queue$Queue, _List_Nil, _List_Nil));
	} else {
		var head = fl.a;
		var tail = fl.b;
		return _Utils_Tuple2(
			$elm$core$Maybe$Just(head),
			A2($elm_explorations$test$Queue$queue, tail, rl));
	}
};
var $elm_explorations$test$RandomRun$nextChoice = function (run) {
	var _v0 = $elm_explorations$test$Queue$dequeue(run.data);
	if (_v0.a.$ === 'Nothing') {
		var _v1 = _v0.a;
		return $elm$core$Maybe$Nothing;
	} else {
		var first = _v0.a.a;
		var rest = _v0.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(
				first,
				_Utils_update(
					run,
					{data: rest, length: run.length - 1})));
	}
};
var $elm_explorations$test$Fuzz$forcedChoice = function (n) {
	return $elm_explorations$test$Fuzz$Internal$Fuzzer(
		function (prng) {
			if (n < 0) {
				return $elm_explorations$test$GenResult$Rejected(
					{prng: prng, reason: 'elm-test bug: forcedChoice: n < 0'});
			} else {
				if (prng.$ === 'Random') {
					var r = prng.a;
					return $elm_explorations$test$GenResult$Generated(
						{
							prng: $elm_explorations$test$PRNG$Random(
								_Utils_update(
									r,
									{
										run: A2($elm_explorations$test$RandomRun$append, n, r.run)
									})),
							value: n
						});
				} else {
					var h = prng.a;
					var _v1 = $elm_explorations$test$RandomRun$nextChoice(h.unusedPart);
					if (_v1.$ === 'Nothing') {
						return $elm_explorations$test$GenResult$Rejected(
							{prng: prng, reason: 'elm-test internals: hardcoded PRNG run out of numbers'});
					} else {
						var _v2 = _v1.a;
						var hardcodedChoice = _v2.a;
						var restOfChoices = _v2.b;
						return (!_Utils_eq(hardcodedChoice, n)) ? $elm_explorations$test$GenResult$Rejected(
							{prng: prng, reason: 'elm-test internals: hardcoded value was not the same as the forced one'}) : $elm_explorations$test$GenResult$Generated(
							{
								prng: $elm_explorations$test$PRNG$Hardcoded(
									_Utils_update(
										h,
										{unusedPart: restOfChoices})),
								value: n
							});
					}
				}
			}
		});
};
var $elm_explorations$test$Fuzz$intToBool = function (n) {
	return (!n) ? false : true;
};
var $elm_explorations$test$Fuzz$map = F2(
	function (fn, _v0) {
		var fuzzer = _v0.a;
		return $elm_explorations$test$Fuzz$Internal$Fuzzer(
			function (prng) {
				var _v1 = fuzzer(prng);
				if (_v1.$ === 'Generated') {
					var g = _v1.a;
					return $elm_explorations$test$GenResult$Generated(
						{
							prng: g.prng,
							value: fn(g.value)
						});
				} else {
					var r = _v1.a;
					return $elm_explorations$test$GenResult$Rejected(r);
				}
			});
	});
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm_explorations$test$Fuzz$rollDice = F2(
	function (maxValue, diceGenerator) {
		return $elm_explorations$test$Fuzz$Internal$Fuzzer(
			function (prng) {
				if (prng.$ === 'Random') {
					var r = prng.a;
					var _v1 = A2($elm$random$Random$step, diceGenerator, r.seed);
					var diceRoll = _v1.a;
					var newSeed = _v1.b;
					return (diceRoll < 0) ? $elm_explorations$test$GenResult$Rejected(
						{prng: prng, reason: 'elm-test bug: generated a choice < 0'}) : ((_Utils_cmp(diceRoll, maxValue) > 0) ? $elm_explorations$test$GenResult$Rejected(
						{prng: prng, reason: 'elm-test bug: generated a choice > maxChoice'}) : $elm_explorations$test$GenResult$Generated(
						{
							prng: $elm_explorations$test$PRNG$Random(
								{
									run: A2($elm_explorations$test$RandomRun$append, diceRoll, r.run),
									seed: newSeed
								}),
							value: diceRoll
						}));
				} else {
					var h = prng.a;
					var _v2 = $elm_explorations$test$RandomRun$nextChoice(h.unusedPart);
					if (_v2.$ === 'Nothing') {
						return $elm_explorations$test$GenResult$Rejected(
							{prng: prng, reason: 'elm-test internals: hardcoded PRNG run out of numbers'});
					} else {
						var _v3 = _v2.a;
						var hardcodedChoice = _v3.a;
						var restOfChoices = _v3.b;
						return (hardcodedChoice < 0) ? $elm_explorations$test$GenResult$Rejected(
							{prng: prng, reason: 'elm-test internals: generated a choice < 0'}) : ((_Utils_cmp(hardcodedChoice, maxValue) > 0) ? $elm_explorations$test$GenResult$Rejected(
							{prng: prng, reason: 'elm-test internals: generated a choice > maxChoice'}) : $elm_explorations$test$GenResult$Generated(
							{
								prng: $elm_explorations$test$PRNG$Hardcoded(
									_Utils_update(
										h,
										{unusedPart: restOfChoices})),
								value: hardcodedChoice
							}));
					}
				}
			});
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm_explorations$test$Fuzz$weightedBoolGenerator = function (p) {
	return A2(
		$elm$random$Random$map,
		function (f) {
			return (_Utils_cmp(f, p) < 1) ? 1 : 0;
		},
		A2($elm$random$Random$float, 0, 1));
};
var $elm_explorations$test$Fuzz$weightedBool = function (p) {
	return A2(
		$elm_explorations$test$Fuzz$map,
		$elm_explorations$test$Fuzz$intToBool,
		(p <= 0) ? $elm_explorations$test$Fuzz$forcedChoice(0) : ((p >= 1) ? $elm_explorations$test$Fuzz$forcedChoice(1) : A2(
			$elm_explorations$test$Fuzz$rollDice,
			1,
			$elm_explorations$test$Fuzz$weightedBoolGenerator(p))));
};
var $elm_explorations$test$Fuzz$listOfLengthBetween = F3(
	function (lo, hi, itemFuzzer) {
		listOfLengthBetween:
		while (true) {
			if (_Utils_cmp(lo, hi) > 0) {
				var $temp$lo = hi,
					$temp$hi = lo,
					$temp$itemFuzzer = itemFuzzer;
				lo = $temp$lo;
				hi = $temp$hi;
				itemFuzzer = $temp$itemFuzzer;
				continue listOfLengthBetween;
			} else {
				if (hi <= 0) {
					return $elm_explorations$test$Fuzz$constant(_List_Nil);
				} else {
					var end = function (acc) {
						return $elm_explorations$test$Fuzz$constant(
							$elm$core$List$reverse(acc));
					};
					var average = lo + (hi / 2);
					var continueProbability = 1 - (1 / (1 + average));
					var addItem = F2(
						function (length, acc) {
							return A2(
								$elm_explorations$test$Fuzz$andThen,
								function (item) {
									return A2(
										go,
										length + 1,
										A2($elm$core$List$cons, item, acc));
								},
								itemFuzzer);
						});
					var go = F2(
						function (length, acc) {
							return (_Utils_cmp(length, lo) < 0) ? A2(
								$elm_explorations$test$Fuzz$andThen,
								function (_v0) {
									return A2(addItem, length, acc);
								},
								$elm_explorations$test$Fuzz$forcedChoice(1)) : (_Utils_eq(length, hi) ? A2(
								$elm_explorations$test$Fuzz$andThen,
								function (_v1) {
									return end(acc);
								},
								$elm_explorations$test$Fuzz$forcedChoice(0)) : A2(
								$elm_explorations$test$Fuzz$andThen,
								function (oneMorePlease) {
									return oneMorePlease ? A2(addItem, length, acc) : end(acc);
								},
								$elm_explorations$test$Fuzz$weightedBool(continueProbability)));
						});
					return A2(go, 0, _List_Nil);
				}
			}
		}
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$pow = _Basics_pow;
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm_explorations$test$Fuzz$uniformInt = function (n) {
	return A2(
		$elm_explorations$test$Fuzz$rollDice,
		n,
		A2($elm$random$Random$int, 0, n));
};
var $elm_explorations$test$Fuzz$intBits = function (bitsCount) {
	return $elm_explorations$test$Fuzz$uniformInt(
		A2($elm$core$Basics$pow, 2, bitsCount) - 1);
};
var $elm_explorations$test$Fuzz$intBucketingThreshold = 255;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $elm_explorations$test$Fuzz$intFrequencyGenerator = F2(
	function (w1, ws) {
		return A2(
			$elm$random$Random$weighted,
			_Utils_Tuple2(w1, 0),
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, w) {
						return _Utils_Tuple2(w, i + 1);
					}),
				ws));
	});
var $elm_explorations$test$Fuzz$invalid = function (reason) {
	return $elm_explorations$test$Fuzz$Internal$Fuzzer(
		function (prng) {
			return $elm_explorations$test$GenResult$Rejected(
				{prng: prng, reason: reason});
		});
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm_explorations$test$Fuzz$intFrequency = function (fuzzers) {
	if (A2(
		$elm$core$List$any,
		function (_v0) {
			var w = _v0.a;
			return w <= 0;
		},
		fuzzers)) {
		return $elm_explorations$test$Fuzz$invalid('intFrequency: Weights cannot be non-positive');
	} else {
		if (fuzzers.b) {
			var _v2 = fuzzers.a;
			var n = _v2.a;
			var rest = fuzzers.b;
			var weightSum = A3(
				$elm$core$List$foldl,
				F2(
					function (_v3, acc) {
						var w = _v3.a;
						return w + acc;
					}),
				n,
				rest);
			return A2(
				$elm_explorations$test$Fuzz$andThen,
				function (i) {
					return A2(
						$elm$core$Maybe$withDefault,
						$elm_explorations$test$Fuzz$invalid('elm-test bug: intFrequency index out of range'),
						A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$second,
							$elm$core$List$head(
								A2($elm$core$List$drop, i, fuzzers))));
				},
				A2(
					$elm_explorations$test$Fuzz$rollDice,
					weightSum - 1,
					A2(
						$elm_explorations$test$Fuzz$intFrequencyGenerator,
						n,
						A2($elm$core$List$map, $elm$core$Tuple$first, rest))));
		} else {
			return $elm_explorations$test$Fuzz$invalid('intFrequency: You must provide at least one item.');
		}
	}
};
var $elm_explorations$test$Fuzz$intPreferences = _List_fromArray(
	[
		{bits: 4, weight: 4},
		{bits: 8, weight: 8},
		{bits: 16, weight: 2},
		{bits: 32, weight: 1}
	]);
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm_explorations$test$MicroListExtra$getAt = F2(
	function (index, list) {
		return (index < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, index, list));
	});
var $elm_explorations$test$Fuzz$oneOfHelp = F3(
	function (functionName, itemName, fuzzers) {
		var _v0 = $elm$core$List$length(fuzzers);
		if (!_v0) {
			return $elm_explorations$test$Fuzz$invalid(functionName + ': You must provide at least one item.');
		} else {
			var length = _v0;
			return A2(
				$elm_explorations$test$Fuzz$andThen,
				function (i) {
					var _v1 = A2($elm_explorations$test$MicroListExtra$getAt, i, fuzzers);
					if (_v1.$ === 'Nothing') {
						return $elm_explorations$test$Fuzz$invalid(
							'elm-test bug: ' + (functionName + (' didn\'t find a ' + (itemName + (' at position ' + ($elm$core$String$fromInt(i) + (' in the list of length ' + ($elm$core$String$fromInt(length) + '.'))))))));
					} else {
						var fuzzer = _v1.a;
						return fuzzer;
					}
				},
				$elm_explorations$test$Fuzz$uniformInt(length - 1));
		}
	});
var $elm_explorations$test$Fuzz$oneOf = function (fuzzers) {
	return A3($elm_explorations$test$Fuzz$oneOfHelp, 'Fuzz.oneOf', 'fuzzer', fuzzers);
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_explorations$test$Fuzz$intRange = F2(
	function (lo, hi) {
		intRange:
		while (true) {
			if (_Utils_cmp(hi, lo) < 0) {
				var $temp$lo = hi,
					$temp$hi = lo;
				lo = $temp$lo;
				hi = $temp$hi;
				continue intRange;
			} else {
				if (_Utils_eq(lo, hi)) {
					return $elm_explorations$test$Fuzz$constant(lo);
				} else {
					var int_ = function (upperLimit) {
						if (_Utils_cmp(upperLimit, $elm_explorations$test$Fuzz$intBucketingThreshold) < 1) {
							return $elm_explorations$test$Fuzz$uniformInt(upperLimit);
						} else {
							var range = upperLimit + 1;
							var maxBits = function (n) {
								return A2($elm$core$Basics$pow, 2, n);
							}(
								$elm$core$Basics$ceiling(
									A2(
										$elm$core$Basics$logBase,
										2,
										$elm$core$Basics$ceiling(
											A2($elm$core$Basics$logBase, 2, range)))));
							return A2(
								$elm_explorations$test$Fuzz$map,
								$elm$core$Basics$modBy(range),
								$elm_explorations$test$Fuzz$intFrequency(
									A2(
										$elm$core$List$map,
										function (_v1) {
											var weight = _v1.weight;
											var bits = _v1.bits;
											return _Utils_Tuple2(
												weight,
												$elm_explorations$test$Fuzz$intBits(bits));
										},
										function (list_) {
											return $elm$core$List$isEmpty(list_) ? A2($elm$core$List$take, 1, $elm_explorations$test$Fuzz$intPreferences) : list_;
										}(
											A2(
												$elm$core$List$filter,
												function (_v0) {
													var bits = _v0.bits;
													return _Utils_cmp(bits, maxBits) < 1;
												},
												$elm_explorations$test$Fuzz$intPreferences)))));
						}
					};
					return (lo >= 0) ? A2(
						$elm_explorations$test$Fuzz$map,
						function (n) {
							return n + lo;
						},
						int_(hi - lo)) : ((hi <= 0) ? A2(
						$elm_explorations$test$Fuzz$map,
						function (n) {
							return (-n) + hi;
						},
						int_(hi - lo)) : $elm_explorations$test$Fuzz$oneOf(
						_List_fromArray(
							[
								A2($elm_explorations$test$Fuzz$intRange, 0, hi),
								A2($elm_explorations$test$Fuzz$intRange, lo, -1)
							])));
				}
			}
		}
	});
var $author$project$EffectivenessTests$keyFuzzer = A2($elm_explorations$test$Fuzz$intRange, 0, 10);
var $elm_explorations$test$Fuzz$map2 = F3(
	function (fn, _v0, _v1) {
		var fuzzerA = _v0.a;
		var fuzzerB = _v1.a;
		return $elm_explorations$test$Fuzz$Internal$Fuzzer(
			function (prng) {
				var _v2 = fuzzerA(prng);
				if (_v2.$ === 'Generated') {
					var a = _v2.a;
					var _v3 = fuzzerB(a.prng);
					if (_v3.$ === 'Generated') {
						var b = _v3.a;
						return $elm_explorations$test$GenResult$Generated(
							{
								prng: b.prng,
								value: A2(fn, a.value, b.value)
							});
					} else {
						var r = _v3.a;
						return $elm_explorations$test$GenResult$Rejected(r);
					}
				} else {
					var r = _v2.a;
					return $elm_explorations$test$GenResult$Rejected(r);
				}
			});
	});
var $elm_explorations$test$Fuzz$pair = F2(
	function (fuzzerA, fuzzerB) {
		return A3(
			$elm_explorations$test$Fuzz$map2,
			F2(
				function (a, b) {
					return _Utils_Tuple2(a, b);
				}),
			fuzzerA,
			fuzzerB);
	});
var $author$project$EffectivenessTests$valueFuzzer = A2($elm_explorations$test$Fuzz$intRange, 0, 10);
var $author$project$EffectivenessTests$pairFuzzer = A2($elm_explorations$test$Fuzz$pair, $author$project$EffectivenessTests$keyFuzzer, $author$project$EffectivenessTests$valueFuzzer);
var $author$project$EffectivenessTests$bstFuzzer = A2(
	$elm_explorations$test$Fuzz$map,
	$author$project$EffectivenessSUT$Bst$fromList,
	A3($elm_explorations$test$Fuzz$listOfLengthBetween, 0, 20, $author$project$EffectivenessTests$pairFuzzer));
var $author$project$EffectivenessSUT$Bst$deleteMin = function (tree) {
	if (tree.$ === 'Leaf') {
		return $author$project$EffectivenessSUT$BstCommon$Leaf;
	} else {
		if (tree.a.$ === 'Leaf') {
			var _v1 = tree.a;
			var right = tree.d;
			return right;
		} else {
			var left = tree.a;
			var k = tree.b;
			var v = tree.c;
			var right = tree.d;
			return A4(
				$author$project$EffectivenessSUT$BstCommon$Node,
				$author$project$EffectivenessSUT$Bst$deleteMin(left),
				k,
				v,
				right);
		}
	}
};
var $author$project$EffectivenessSUT$Bst$minNode = function (tree) {
	minNode:
	while (true) {
		if (tree.$ === 'Leaf') {
			return $elm$core$Maybe$Nothing;
		} else {
			if (tree.a.$ === 'Leaf') {
				var _v1 = tree.a;
				var k = tree.b;
				var v = tree.c;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(k, v));
			} else {
				var left = tree.a;
				var $temp$tree = left;
				tree = $temp$tree;
				continue minNode;
			}
		}
	}
};
var $author$project$EffectivenessSUT$Bst$delete = F2(
	function (k, tree) {
		if (tree.$ === 'Leaf') {
			return $author$project$EffectivenessSUT$BstCommon$Leaf;
		} else {
			var left = tree.a;
			var key = tree.b;
			var val = tree.c;
			var right = tree.d;
			if (_Utils_cmp(k, key) < 0) {
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst$delete, k, left),
					key,
					val,
					right);
			} else {
				if (_Utils_cmp(k, key) > 0) {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst$delete, k, right));
				} else {
					var _v1 = _Utils_Tuple2(left, right);
					if (_v1.a.$ === 'Leaf') {
						var _v2 = _v1.a;
						return right;
					} else {
						if (_v1.b.$ === 'Leaf') {
							var _v3 = _v1.b;
							return left;
						} else {
							var _v4 = $author$project$EffectivenessSUT$Bst$minNode(right);
							if (_v4.$ === 'Nothing') {
								return tree;
							} else {
								var _v5 = _v4.a;
								var succKey = _v5.a;
								var succVal = _v5.b;
								var newRight = $author$project$EffectivenessSUT$Bst$deleteMin(right);
								return A4($author$project$EffectivenessSUT$BstCommon$Node, left, succKey, succVal, newRight);
							}
						}
					}
				}
			}
		}
	});
var $elm_explorations$test$Test$Runner$Failure$Equality = F2(
	function (a, b) {
		return {$: 'Equality', a: a, b: b};
	});
var $elm$core$String$contains = _String_contains;
var $elm_explorations$test$Test$Runner$Failure$Custom = {$: 'Custom'};
var $elm_explorations$test$Expect$fail = function (str) {
	return $elm_explorations$test$Test$Expectation$fail(
		{description: str, reason: $elm_explorations$test$Test$Runner$Failure$Custom});
};
var $elm$core$Basics$not = _Basics_not;
var $elm_explorations$test$Test$Expectation$Pass = function (a) {
	return {$: 'Pass', a: a};
};
var $elm_explorations$test$Expect$pass = $elm_explorations$test$Test$Expectation$Pass(
	{distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution});
var $elm_explorations$test$Test$Internal$toString = _Debug_toString;
var $elm_explorations$test$Expect$testWith = F5(
	function (makeReason, label, runTest, expected, actual) {
		return A2(runTest, actual, expected) ? $elm_explorations$test$Expect$pass : $elm_explorations$test$Test$Expectation$fail(
			{
				description: label,
				reason: A2(
					makeReason,
					$elm_explorations$test$Test$Internal$toString(expected),
					$elm_explorations$test$Test$Internal$toString(actual))
			});
	});
var $elm$core$String$toFloat = _String_toFloat;
var $elm$core$String$toInt = _String_toInt;
var $elm_explorations$test$Expect$equateWith = F4(
	function (reason, comparison, b, a) {
		var isJust = function (x) {
			if (x.$ === 'Just') {
				return true;
			} else {
				return false;
			}
		};
		var isFloat = function (x) {
			return isJust(
				$elm$core$String$toFloat(x)) && (!isJust(
				$elm$core$String$toInt(x)));
		};
		var usesFloats = isFloat(
			$elm_explorations$test$Test$Internal$toString(a)) || isFloat(
			$elm_explorations$test$Test$Internal$toString(b));
		var floatError = A2($elm$core$String$contains, reason, 'not') ? 'Do not use Expect.notEqual with floats. Use Expect.notWithin instead.' : 'Do not use Expect.equal with floats. Use Expect.within instead.';
		return usesFloats ? $elm_explorations$test$Expect$fail(floatError) : A5($elm_explorations$test$Expect$testWith, $elm_explorations$test$Test$Runner$Failure$Equality, reason, comparison, b, a);
	});
var $elm_explorations$test$Expect$equal = A2($elm_explorations$test$Expect$equateWith, 'Expect.equal', $elm$core$Basics$eq);
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm_explorations$test$Test$Distribution$Internal$NoDistributionNeeded = {$: 'NoDistributionNeeded'};
var $elm_explorations$test$Test$Internal$blankDescriptionFailure = $elm_explorations$test$Test$Internal$failNow(
	{
		description: 'This test has a blank description. Let\'s give it a useful one!',
		reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
	});
var $elm_explorations$test$Test$Internal$ElmTestVariant__FuzzTest = function (a) {
	return {$: 'ElmTestVariant__FuzzTest', a: a};
};
var $elm_explorations$test$Test$Expectation$withGiven = F2(
	function (newGiven, expectation) {
		if (expectation.$ === 'Fail') {
			var failure = expectation.a;
			return $elm_explorations$test$Test$Expectation$Fail(
				_Utils_update(
					failure,
					{
						given: $elm$core$Maybe$Just(newGiven)
					}));
		} else {
			return expectation;
		}
	});
var $elm_explorations$test$Test$Fuzz$formatExpectation = function (_v0) {
	var given = _v0.given;
	var expectation = _v0.expectation;
	if (given.$ === 'Nothing') {
		return expectation;
	} else {
		var given_ = given.a;
		return A2($elm_explorations$test$Test$Expectation$withGiven, given_, expectation);
	}
};
var $elm_explorations$test$Test$Distribution$DistributionCheckSucceeded = function (a) {
	return {$: 'DistributionCheckSucceeded', a: a};
};
var $elm_explorations$test$Test$Distribution$DistributionToReport = function (a) {
	return {$: 'DistributionToReport', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm_explorations$test$Test$Distribution$Internal$getExpectedDistributions = function (distribution) {
	switch (distribution.$) {
		case 'NoDistributionNeeded':
			return $elm$core$Maybe$Nothing;
		case 'ReportDistribution':
			return $elm$core$Maybe$Nothing;
		default:
			var list = distribution.a;
			return $elm$core$Maybe$Just(
				A2(
					$elm$core$List$map,
					function (_v1) {
						var e = _v1.a;
						var l = _v1.b;
						return _Utils_Tuple2(l, e);
					},
					list));
	}
};
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm_explorations$test$Test$Distribution$Internal$certainty = A2($elm$core$Basics$pow, 10, 9);
var $elm_explorations$test$Test$Distribution$Internal$falsePositiveProb = 1 / $elm_explorations$test$Test$Distribution$Internal$certainty;
var $elm_explorations$test$Test$Distribution$Internal$tolerance = 0.9;
var $elm_explorations$test$Test$Distribution$Internal$a1 = -3.969683028665376e1;
var $elm_explorations$test$Test$Distribution$Internal$a2 = 2.209460984245205e2;
var $elm_explorations$test$Test$Distribution$Internal$a3 = -2.759285104469687e2;
var $elm_explorations$test$Test$Distribution$Internal$a4 = 1.38357751867269e2;
var $elm_explorations$test$Test$Distribution$Internal$a5 = -3.066479806614716e1;
var $elm_explorations$test$Test$Distribution$Internal$a6 = 2.506628277459239e0;
var $elm_explorations$test$Test$Distribution$Internal$b1 = -5.447609879822406e1;
var $elm_explorations$test$Test$Distribution$Internal$b2 = 1.615858368580409e2;
var $elm_explorations$test$Test$Distribution$Internal$b3 = -1.556989798598866e2;
var $elm_explorations$test$Test$Distribution$Internal$b4 = 6.680131188771972e1;
var $elm_explorations$test$Test$Distribution$Internal$b5 = -1.328068155288572e1;
var $elm_explorations$test$Test$Distribution$Internal$c1 = -7.784894002430293e-3;
var $elm_explorations$test$Test$Distribution$Internal$c2 = -3.223964580411365e-1;
var $elm_explorations$test$Test$Distribution$Internal$c3 = -2.400758277161838e0;
var $elm_explorations$test$Test$Distribution$Internal$c4 = -2.549732539343734e0;
var $elm_explorations$test$Test$Distribution$Internal$c5 = 4.374664141464968e0;
var $elm_explorations$test$Test$Distribution$Internal$c6 = 2.938163982698783e0;
var $elm_explorations$test$Test$Distribution$Internal$d1 = 7.784695709041462e-3;
var $elm_explorations$test$Test$Distribution$Internal$d2 = 3.224671290700398e-1;
var $elm_explorations$test$Test$Distribution$Internal$d3 = 2.445134137142996e0;
var $elm_explorations$test$Test$Distribution$Internal$d4 = 3.754408661907416e0;
var $elm$core$Basics$e = _Basics_e;
var $elm_explorations$test$Test$Distribution$Internal$pLow = 0.02425;
var $elm_explorations$test$Test$Distribution$Internal$pHigh = 1 - $elm_explorations$test$Test$Distribution$Internal$pLow;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $elm_explorations$test$Test$Distribution$Internal$invnormcdf = function (p) {
	if (p < 0) {
		return 0 / 0;
	} else {
		if (p > 1) {
			return 0 / 0;
		} else {
			if (!p) {
				return (-1) / 0;
			} else {
				if (p === 1) {
					return 1 / 0;
				} else {
					if (_Utils_cmp(p, $elm_explorations$test$Test$Distribution$Internal$pLow) < 0) {
						var q = $elm$core$Basics$sqrt(
							(-2) * A2($elm$core$Basics$logBase, $elm$core$Basics$e, p));
						return (((((((((($elm_explorations$test$Test$Distribution$Internal$c1 * q) + $elm_explorations$test$Test$Distribution$Internal$c2) * q) + $elm_explorations$test$Test$Distribution$Internal$c3) * q) + $elm_explorations$test$Test$Distribution$Internal$c4) * q) + $elm_explorations$test$Test$Distribution$Internal$c5) * q) + $elm_explorations$test$Test$Distribution$Internal$c6) / (((((((($elm_explorations$test$Test$Distribution$Internal$d1 * q) + $elm_explorations$test$Test$Distribution$Internal$d2) * q) + $elm_explorations$test$Test$Distribution$Internal$d3) * q) + $elm_explorations$test$Test$Distribution$Internal$d4) * q) + 1);
					} else {
						if (_Utils_cmp(p, $elm_explorations$test$Test$Distribution$Internal$pHigh) < 1) {
							var q = p - 0.5;
							var r = q * q;
							return ((((((((((($elm_explorations$test$Test$Distribution$Internal$a1 * r) + $elm_explorations$test$Test$Distribution$Internal$a2) * r) + $elm_explorations$test$Test$Distribution$Internal$a3) * r) + $elm_explorations$test$Test$Distribution$Internal$a4) * r) + $elm_explorations$test$Test$Distribution$Internal$a5) * r) + $elm_explorations$test$Test$Distribution$Internal$a6) * q) / (((((((((($elm_explorations$test$Test$Distribution$Internal$b1 * r) + $elm_explorations$test$Test$Distribution$Internal$b2) * r) + $elm_explorations$test$Test$Distribution$Internal$b3) * r) + $elm_explorations$test$Test$Distribution$Internal$b4) * r) + $elm_explorations$test$Test$Distribution$Internal$b5) * r) + 1);
						} else {
							var q = $elm$core$Basics$sqrt(
								(-2) * A2($elm$core$Basics$logBase, $elm$core$Basics$e, 1 - p));
							return (-(((((((((($elm_explorations$test$Test$Distribution$Internal$c1 * q) + $elm_explorations$test$Test$Distribution$Internal$c2) * q) + $elm_explorations$test$Test$Distribution$Internal$c3) * q) + $elm_explorations$test$Test$Distribution$Internal$c4) * q) + $elm_explorations$test$Test$Distribution$Internal$c5) * q) + $elm_explorations$test$Test$Distribution$Internal$c6)) / (((((((($elm_explorations$test$Test$Distribution$Internal$d1 * q) + $elm_explorations$test$Test$Distribution$Internal$d2) * q) + $elm_explorations$test$Test$Distribution$Internal$d3) * q) + $elm_explorations$test$Test$Distribution$Internal$d4) * q) + 1);
						}
					}
				}
			}
		}
	}
};
var $elm_explorations$test$Test$Distribution$Internal$wilson = F3(
	function (k, n, z) {
		var zz = z * z;
		var p = k / n;
		return ((p + (zz / (2 * n))) + (z * $elm$core$Basics$sqrt(((p * (1 - p)) / n) + (zz / ((4 * n) * n))))) / (1 + (zz / n));
	});
var $elm_explorations$test$Test$Distribution$Internal$wilsonLow = F3(
	function (seen, total, prob) {
		return A3(
			$elm_explorations$test$Test$Distribution$Internal$wilson,
			seen,
			total,
			$elm_explorations$test$Test$Distribution$Internal$invnormcdf(prob / 2));
	});
var $elm_explorations$test$Test$Distribution$Internal$sufficientlyCovered = F3(
	function (total, seen, percentage) {
		return _Utils_cmp(
			A3($elm_explorations$test$Test$Distribution$Internal$wilsonLow, seen, total, $elm_explorations$test$Test$Distribution$Internal$falsePositiveProb),
			$elm_explorations$test$Test$Distribution$Internal$tolerance * percentage) > -1;
	});
var $elm_explorations$test$MicroMaybeExtra$traverseHelp = F3(
	function (f, list, acc) {
		traverseHelp:
		while (true) {
			if (list.b) {
				var head = list.a;
				var tail = list.b;
				var _v1 = f(head);
				if (_v1.$ === 'Just') {
					var a = _v1.a;
					var $temp$f = f,
						$temp$list = tail,
						$temp$acc = A2($elm$core$List$cons, a, acc);
					f = $temp$f;
					list = $temp$list;
					acc = $temp$acc;
					continue traverseHelp;
				} else {
					return $elm$core$Maybe$Nothing;
				}
			} else {
				return $elm$core$Maybe$Just(
					$elm$core$List$reverse(acc));
			}
		}
	});
var $elm_explorations$test$MicroMaybeExtra$traverse = F2(
	function (f, list) {
		return A3($elm_explorations$test$MicroMaybeExtra$traverseHelp, f, list, _List_Nil);
	});
var $elm_explorations$test$Test$Fuzz$allSufficientlyCovered = F3(
	function (c, state, normalizedDistributionCount) {
		return A2(
			$elm$core$Maybe$withDefault,
			false,
			A2(
				$elm$core$Maybe$andThen,
				function (_v0) {
					var distributionCount = _v0.a;
					var expectedDistributions = _v0.b;
					var expectedDistributions_ = $elm$core$Dict$fromList(expectedDistributions);
					return A2(
						$elm$core$Maybe$map,
						$elm$core$List$all(
							function (_v4) {
								var count = _v4.b;
								var expectedDistribution = _v4.c;
								switch (expectedDistribution.$) {
									case 'Zero':
										return true;
									case 'MoreThanZero':
										return true;
									default:
										var n = expectedDistribution.a;
										return A3($elm_explorations$test$Test$Distribution$Internal$sufficientlyCovered, state.runsElapsed, count, n / 100);
								}
							}),
						A2(
							$elm_explorations$test$MicroMaybeExtra$traverse,
							function (_v3) {
								var labels = _v3.a;
								var count = _v3.b;
								return A2(
									$elm$core$Maybe$map,
									function (expectedDistribution) {
										return _Utils_Tuple3(labels, count, expectedDistribution);
									},
									A2($elm$core$Dict$get, labels, expectedDistributions_));
							},
							A2(
								$elm$core$List$filterMap,
								function (_v1) {
									var labels = _v1.a;
									var count = _v1.b;
									if (labels.b && (!labels.b.b)) {
										var onlyLabel = labels.a;
										return $elm$core$Maybe$Just(
											_Utils_Tuple2(onlyLabel, count));
									} else {
										return $elm$core$Maybe$Nothing;
									}
								},
								$elm$core$Dict$toList(distributionCount))));
				},
				A3(
					$elm$core$Maybe$map2,
					$elm$core$Tuple$pair,
					normalizedDistributionCount,
					$elm_explorations$test$Test$Distribution$Internal$getExpectedDistributions(c.distribution))));
	});
var $elm_explorations$test$Test$Runner$Failure$DistributionBug = {$: 'DistributionBug'};
var $elm_explorations$test$Test$Fuzz$distributionBugRunResult = {
	distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution,
	failure: $elm$core$Maybe$Just(
		{
			expectation: $elm_explorations$test$Test$Expectation$fail(
				{
					description: 'elm-test distribution collection bug',
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DistributionBug)
				}),
			given: $elm$core$Maybe$Nothing
		})
};
var $elm_explorations$test$Test$Distribution$DistributionCheckFailed = function (a) {
	return {$: 'DistributionCheckFailed', a: a};
};
var $elm_explorations$test$Test$Runner$Failure$DistributionInsufficient = {$: 'DistributionInsufficient'};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$Basics$round = _Basics_round;
var $elm_explorations$test$Test$Distribution$Internal$formatPct = function (n) {
	var intPart = $elm$core$Basics$floor(n);
	var thousandths = $elm$core$Basics$round((n * 1000) - (intPart * 1000));
	return $elm$core$String$fromInt(intPart) + ('.' + (A3(
		$elm$core$String$padLeft,
		3,
		_Utils_chr('0'),
		$elm$core$String$fromInt(thousandths)) + '%'));
};
var $elm_explorations$test$Test$Fuzz$formatExpectedDistribution = function (expected) {
	switch (expected.$) {
		case 'Zero':
			return 'exactly 0%';
		case 'MoreThanZero':
			return 'more than 0%';
		default:
			var n = expected.a;
			return $elm_explorations$test$Test$Distribution$Internal$formatPct(n);
	}
};
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm_explorations$test$Test$Fuzz$distributionInsufficientFailure = function (failure) {
	return {
		expectation: $elm_explorations$test$Test$Expectation$fail(
			{
				description: A3(
					$elm$core$String$replace,
					'{RUNS}',
					$elm$core$String$fromInt(failure.runsElapsed),
					A3(
						$elm$core$String$replace,
						'{ACTUAL_PERCENTAGE}',
						$elm_explorations$test$Test$Distribution$Internal$formatPct(failure.actualPercentage),
						A3(
							$elm$core$String$replace,
							'{EXPECTED_PERCENTAGE}',
							$elm_explorations$test$Test$Fuzz$formatExpectedDistribution(failure.expectedDistribution),
							A3($elm$core$String$replace, '{LABEL}', failure.label, 'Distribution of label "{LABEL}" was insufficient:\n  expected:  {EXPECTED_PERCENTAGE}\n  got:       {ACTUAL_PERCENTAGE}.\n\n(Generated {RUNS} values.)')))),
				reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DistributionInsufficient)
			}),
		given: $elm$core$Maybe$Nothing
	};
};
var $elm_explorations$test$Test$Distribution$Internal$expectedDistributionToString = function (expectedDistribution) {
	switch (expectedDistribution.$) {
		case 'Zero':
			return '0%';
		case 'MoreThanZero':
			return '> 0%';
		default:
			var pct = expectedDistribution.a;
			return '>= ' + $elm_explorations$test$Test$Distribution$Internal$formatPct(pct);
	}
};
var $elm_explorations$test$Test$Fuzz$distributionFailRunResult = F2(
	function (normalizedDistributionCount, failedLabel) {
		if (normalizedDistributionCount.$ === 'Nothing') {
			return $elm_explorations$test$Test$Fuzz$distributionBugRunResult;
		} else {
			var distributionCount = normalizedDistributionCount.a;
			return {
				distributionReport: $elm_explorations$test$Test$Distribution$DistributionCheckFailed(
					{
						badLabel: failedLabel.label,
						badLabelPercentage: failedLabel.actualPercentage,
						distributionCount: distributionCount,
						expectedDistribution: $elm_explorations$test$Test$Distribution$Internal$expectedDistributionToString(failedLabel.expectedDistribution),
						runsElapsed: failedLabel.runsElapsed
					}),
				failure: $elm$core$Maybe$Just(
					$elm_explorations$test$Test$Fuzz$distributionInsufficientFailure(failedLabel))
			};
		}
	});
var $elm_explorations$test$MicroListExtra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm_explorations$test$Test$Fuzz$findBadZeroRelatedCase = F3(
	function (c, state, normalizedDistributionCount) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var distributionCount = _v0.a;
				var expectedDistributions = _v0.b;
				return A2(
					$elm$core$Maybe$andThen,
					function (_v3) {
						var label = _v3.a;
						var expectedDistribution = _v3.b;
						return A2(
							$elm$core$Maybe$map,
							function (count) {
								return {actualPercentage: (count * 100) / state.runsElapsed, distributionCount: distributionCount, expectedDistribution: expectedDistribution, label: label, runsElapsed: state.runsElapsed};
							},
							A2(
								$elm$core$Dict$get,
								_List_fromArray(
									[label]),
								distributionCount));
					},
					A2(
						$elm_explorations$test$MicroListExtra$find,
						function (_v1) {
							var label = _v1.a;
							var expectedDistribution = _v1.b;
							switch (expectedDistribution.$) {
								case 'Zero':
									return !(!A2(
										$elm$core$Maybe$withDefault,
										1,
										A2(
											$elm$core$Dict$get,
											_List_fromArray(
												[label]),
											distributionCount)));
								case 'MoreThanZero':
									return !A2(
										$elm$core$Maybe$withDefault,
										0,
										A2(
											$elm$core$Dict$get,
											_List_fromArray(
												[label]),
											distributionCount));
								default:
									return false;
							}
						},
						expectedDistributions));
			},
			A3(
				$elm$core$Maybe$map2,
				$elm$core$Tuple$pair,
				normalizedDistributionCount,
				$elm_explorations$test$Test$Distribution$Internal$getExpectedDistributions(c.distribution)));
	});
var $elm_explorations$test$Test$Distribution$Internal$wilsonHigh = F3(
	function (seen, total, prob) {
		return A3(
			$elm_explorations$test$Test$Distribution$Internal$wilson,
			seen,
			total,
			$elm_explorations$test$Test$Distribution$Internal$invnormcdf(1 - (prob / 2)));
	});
var $elm_explorations$test$Test$Distribution$Internal$insufficientlyCovered = F3(
	function (total, seen, percentage) {
		return _Utils_cmp(
			A3($elm_explorations$test$Test$Distribution$Internal$wilsonHigh, seen, total, $elm_explorations$test$Test$Distribution$Internal$falsePositiveProb),
			percentage) < 0;
	});
var $elm_explorations$test$Test$Fuzz$findInsufficientlyCoveredLabel = F3(
	function (c, state, normalizedDistributionCount) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v0) {
				var distributionCount = _v0.a;
				var expectedDistributions = _v0.b;
				var expectedDistributions_ = $elm$core$Dict$fromList(expectedDistributions);
				return A2(
					$elm$core$Maybe$map,
					function (_v5) {
						var label = _v5.a;
						var count = _v5.b;
						var expectedDistribution = _v5.c;
						return {actualPercentage: (count * 100) / state.runsElapsed, distributionCount: distributionCount, expectedDistribution: expectedDistribution, label: label, runsElapsed: state.runsElapsed};
					},
					A2(
						$elm_explorations$test$MicroListExtra$find,
						function (_v3) {
							var count = _v3.b;
							var expectedDistribution = _v3.c;
							switch (expectedDistribution.$) {
								case 'Zero':
									return false;
								case 'MoreThanZero':
									return false;
								default:
									var n = expectedDistribution.a;
									return A3($elm_explorations$test$Test$Distribution$Internal$insufficientlyCovered, state.runsElapsed, count, n / 100);
							}
						},
						A2(
							$elm$core$List$filterMap,
							function (_v1) {
								var labels = _v1.a;
								var count = _v1.b;
								if (labels.b && (!labels.b.b)) {
									var onlyLabel = labels.a;
									return A2(
										$elm$core$Maybe$map,
										function (expectedDistribution) {
											return _Utils_Tuple3(onlyLabel, count, expectedDistribution);
										},
										A2($elm$core$Dict$get, onlyLabel, expectedDistributions_));
								} else {
									return $elm$core$Maybe$Nothing;
								}
							},
							$elm$core$Dict$toList(distributionCount))));
			},
			A3(
				$elm$core$Maybe$map2,
				$elm$core$Tuple$pair,
				normalizedDistributionCount,
				$elm_explorations$test$Test$Distribution$Internal$getExpectedDistributions(c.distribution)));
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm_explorations$test$Test$Fuzz$includeCombinationsInBaseCounts = function (distribution) {
	return A2(
		$elm$core$Dict$map,
		F2(
			function (labels, count) {
				if (labels.b && (!labels.b.b)) {
					var single = labels.a;
					var combinations = $elm$core$Dict$values(
						A2(
							$elm$core$Dict$filter,
							F2(
								function (k, _v1) {
									return ($elm$core$List$length(k) > 1) && A2($elm$core$List$member, single, k);
								}),
							distribution));
					return count + $elm$core$List$sum(combinations);
				} else {
					return count;
				}
			}),
		distribution);
};
var $elm_explorations$test$Test$Runner$Failure$InvalidFuzzer = {$: 'InvalidFuzzer'};
var $elm_explorations$test$Fuzz$Internal$generate = F2(
	function (prng, _v0) {
		var fuzzer = _v0.a;
		return fuzzer(prng);
	});
var $elm_explorations$test$Test$Distribution$Internal$getDistributionLabels = function (distribution) {
	switch (distribution.$) {
		case 'NoDistributionNeeded':
			return $elm$core$Maybe$Nothing;
		case 'ReportDistribution':
			var list = distribution.a;
			return $elm$core$Maybe$Just(list);
		default:
			var list = distribution.a;
			return $elm$core$Maybe$Just(
				A2(
					$elm$core$List$map,
					function (_v1) {
						var l = _v1.b;
						var p = _v1.c;
						return _Utils_Tuple2(l, p);
					},
					list));
	}
};
var $elm_explorations$test$GenResult$getPrng = function (genResult) {
	if (genResult.$ === 'Generated') {
		var prng = genResult.a.prng;
		return prng;
	} else {
		var prng = genResult.a.prng;
		return prng;
	}
};
var $elm_explorations$test$PRNG$getRun = function (prng) {
	if (prng.$ === 'Random') {
		var run = prng.a.run;
		return run;
	} else {
		var wholeRun = prng.a.wholeRun;
		return wholeRun;
	}
};
var $elm_explorations$test$PRNG$getSeed = function (prng) {
	if (prng.$ === 'Random') {
		var seed = prng.a.seed;
		return $elm$core$Maybe$Just(seed);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm_explorations$test$MicroDictExtra$increment = F2(
	function (key, dict) {
		return A3(
			$elm$core$Dict$update,
			key,
			function (maybeValue) {
				if (maybeValue.$ === 'Nothing') {
					return $elm$core$Maybe$Just(1);
				} else {
					var value = maybeValue.a;
					return $elm$core$Maybe$Just(value + 1);
				}
			},
			dict);
	});
var $elm_explorations$test$Queue$empty = A2($elm_explorations$test$Queue$Queue, _List_Nil, _List_Nil);
var $elm_explorations$test$RandomRun$empty = {data: $elm_explorations$test$Queue$empty, length: 0};
var $elm_explorations$test$PRNG$random = function (seed) {
	return $elm_explorations$test$PRNG$Random(
		{run: $elm_explorations$test$RandomRun$empty, seed: seed});
};
var $elm_explorations$test$Test$Fuzz$stepSeed = function (seed) {
	return A2(
		$elm$random$Random$step,
		A2($elm$random$Random$int, 0, 0),
		seed).b;
};
var $elm_explorations$test$RandomRun$isEmpty = function (run) {
	return !run.length;
};
var $elm_explorations$test$Queue$toList = function (_v0) {
	var fl = _v0.a;
	var rl = _v0.b;
	return _Utils_ap(
		fl,
		$elm$core$List$reverse(rl));
};
var $elm_explorations$test$RandomRun$toList = function (run) {
	return $elm_explorations$test$Queue$toList(run.data);
};
var $elm_explorations$test$Simplify$logRun = F2(
	function (label, run) {
		var _v0 = A2(
			$elm$core$Debug$log,
			label,
			$elm_explorations$test$RandomRun$toList(run));
		return run;
	});
var $elm_explorations$test$DebugConfig$shouldLogFirstFailure = false;
var $elm_explorations$test$RandomRun$equal = F2(
	function (run1, run2) {
		return _Utils_eq(
			$elm_explorations$test$RandomRun$toList(run1),
			$elm_explorations$test$RandomRun$toList(run2));
	});
var $elm_explorations$test$PRNG$hardcoded = function (run) {
	return $elm_explorations$test$PRNG$Hardcoded(
		{unusedPart: run, wholeRun: run});
};
var $elm$core$Debug$toString = _Debug_toString;
var $elm_explorations$test$Simplify$logState = F2(
	function (label, state) {
		var runString = $elm$core$Debug$toString(
			$elm_explorations$test$RandomRun$toList(state.randomRun));
		var _v0 = function () {
			var _v1 = A2(
				$elm_explorations$test$Fuzz$Internal$generate,
				$elm_explorations$test$PRNG$hardcoded(state.randomRun),
				state.fuzzer);
			if (_v1.$ === 'Generated') {
				var value = _v1.a.value;
				var _v2 = A2($elm$core$Debug$log, label + (' - ' + (runString + ' --->')), value);
				return _Utils_Tuple0;
			} else {
				return _Utils_Tuple0;
			}
		}();
		return state;
	});
var $elm_explorations$test$DebugConfig$shouldLogShrinkProgress = false;
var $elm_explorations$test$Simplify$Cmd$DecrementTogether = function (a) {
	return {$: 'DecrementTogether', a: a};
};
var $elm_explorations$test$MicroListExtra$fastConcatMap = function (f) {
	return A2(
		$elm$core$List$foldr,
		F2(
			function (e, a) {
				return _Utils_ap(
					f(e),
					a);
			}),
		_List_Nil);
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm_explorations$test$Simplify$Cmd$decrementTogetherCmds = function (length) {
	var maxOffsetLimit = (length < 512) ? 4 : 2;
	return A2(
		$elm_explorations$test$MicroListExtra$fastConcatMap,
		function (index) {
			var maxOffset = A2($elm$core$Basics$min, maxOffsetLimit, (length - index) - 1);
			return A2(
				$elm_explorations$test$MicroListExtra$fastConcatMap,
				function (offset) {
					return A2(
						$elm$core$List$map,
						function (by) {
							var rightIndex = index + offset;
							return {
								minLength: rightIndex + 1,
								type_: $elm_explorations$test$Simplify$Cmd$DecrementTogether(
									{by: by, leftIndex: index, rightIndex: rightIndex})
							};
						},
						_List_fromArray(
							[4, 2, 1]));
				},
				A2($elm$core$List$range, 1, maxOffset));
		},
		A2($elm$core$List$range, 0, length - 2));
};
var $elm_explorations$test$Simplify$Cmd$DeleteChunkAndMaybeDecrementPrevious = function (a) {
	return {$: 'DeleteChunkAndMaybeDecrementPrevious', a: a};
};
var $elm_explorations$test$Simplify$Cmd$chunkCmds = F2(
	function (toType, _v0) {
		var length = _v0.length;
		var allowChunksOfSize1 = _v0.allowChunksOfSize1;
		var initChunkSize = allowChunksOfSize1 ? 1 : 2;
		var go = F3(
			function (chunkSize, startIndex, acc) {
				go:
				while (true) {
					if (_Utils_cmp(startIndex, length - chunkSize) > 0) {
						if (chunkSize === 8) {
							return acc;
						} else {
							if ((chunkSize === 2) || (chunkSize === 3)) {
								var $temp$chunkSize = chunkSize + 1,
									$temp$startIndex = 0,
									$temp$acc = acc;
								chunkSize = $temp$chunkSize;
								startIndex = $temp$startIndex;
								acc = $temp$acc;
								continue go;
							} else {
								var $temp$chunkSize = chunkSize * 2,
									$temp$startIndex = 0,
									$temp$acc = acc;
								chunkSize = $temp$chunkSize;
								startIndex = $temp$startIndex;
								acc = $temp$acc;
								continue go;
							}
						}
					} else {
						var newCmd = {
							minLength: startIndex + chunkSize,
							type_: toType(
								{size: chunkSize, startIndex: startIndex})
						};
						var $temp$chunkSize = chunkSize,
							$temp$startIndex = startIndex + 1,
							$temp$acc = A2($elm$core$List$cons, newCmd, acc);
						chunkSize = $temp$chunkSize;
						startIndex = $temp$startIndex;
						acc = $temp$acc;
						continue go;
					}
				}
			});
		return A3(go, initChunkSize, 0, _List_Nil);
	});
var $elm_explorations$test$Simplify$Cmd$deletionCmds = function (length) {
	return A2(
		$elm_explorations$test$Simplify$Cmd$chunkCmds,
		$elm_explorations$test$Simplify$Cmd$DeleteChunkAndMaybeDecrementPrevious,
		{allowChunksOfSize1: true, length: length});
};
var $elm_explorations$test$MicroListExtra$fastConcat = A2($elm$core$List$foldr, $elm$core$Basics$append, _List_Nil);
var $elm_explorations$test$RandomRun$length = function (run) {
	return run.length;
};
var $elm_explorations$test$Simplify$Cmd$MinimizeChoice = function (a) {
	return {$: 'MinimizeChoice', a: a};
};
var $elm_explorations$test$Simplify$Cmd$minimizeChoiceCmds = F2(
	function (run, length) {
		return A2(
			$elm$core$List$filterMap,
			function (_v0) {
				var index = _v0.a;
				var value = _v0.b;
				return (value > 0) ? $elm$core$Maybe$Just(
					{
						minLength: index + 1,
						type_: $elm_explorations$test$Simplify$Cmd$MinimizeChoice(
							{index: index})
					}) : $elm$core$Maybe$Nothing;
			},
			A2(
				$elm$core$List$indexedMap,
				$elm$core$Tuple$pair,
				$elm_explorations$test$RandomRun$toList(run)));
	});
var $elm_explorations$test$Simplify$Cmd$MinimizeFloat = function (a) {
	return {$: 'MinimizeFloat', a: a};
};
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm_explorations$test$Simplify$Cmd$minimizeFloatCmds = F2(
	function (run, length) {
		var possibleBoolIndexes = $elm$core$Set$fromList(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var index = _v0.a;
					var value = _v0.b;
					return (value > 1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(index);
				},
				A2(
					$elm$core$List$indexedMap,
					$elm$core$Tuple$pair,
					$elm_explorations$test$RandomRun$toList(run))));
		return A2(
			$elm$core$List$filterMap,
			function (index) {
				return A2($elm$core$Set$member, index + 2, possibleBoolIndexes) ? $elm$core$Maybe$Just(
					{
						minLength: index + 3,
						type_: $elm_explorations$test$Simplify$Cmd$MinimizeFloat(
							{leftIndex: index})
					}) : $elm$core$Maybe$Nothing;
			},
			A2($elm$core$List$range, 0, length - 3));
	});
var $elm_explorations$test$Simplify$Cmd$RedistributeChoicesAndMaybeIncrement = function (a) {
	return {$: 'RedistributeChoicesAndMaybeIncrement', a: a};
};
var $elm_explorations$test$Simplify$Cmd$redistributeCmds = function (length) {
	var forOffset = function (offset) {
		return (_Utils_cmp(offset, length) > -1) ? _List_Nil : A2(
			$elm$core$List$map,
			function (leftIndex) {
				return {
					minLength: (leftIndex + offset) + 1,
					type_: $elm_explorations$test$Simplify$Cmd$RedistributeChoicesAndMaybeIncrement(
						{leftIndex: leftIndex, rightIndex: leftIndex + offset})
				};
			},
			$elm$core$List$reverse(
				A2($elm$core$List$range, 0, (length - 1) - offset)));
	};
	return _Utils_ap(
		forOffset(3),
		_Utils_ap(
			forOffset(2),
			forOffset(1)));
};
var $elm_explorations$test$Simplify$Cmd$SortChunk = function (a) {
	return {$: 'SortChunk', a: a};
};
var $elm_explorations$test$Simplify$Cmd$sortCmds = function (length) {
	return A2(
		$elm_explorations$test$Simplify$Cmd$chunkCmds,
		$elm_explorations$test$Simplify$Cmd$SortChunk,
		{allowChunksOfSize1: false, length: length});
};
var $elm_explorations$test$Simplify$Cmd$SwapChunkWithNeighbour = function (a) {
	return {$: 'SwapChunkWithNeighbour', a: a};
};
var $elm_explorations$test$Simplify$Cmd$swapCmds = function (length) {
	return A2(
		$elm$core$List$map,
		function (cmd) {
			var _v0 = cmd.type_;
			if (_v0.$ === 'SwapChunkWithNeighbour') {
				var chunk = _v0.a;
				return _Utils_update(
					cmd,
					{minLength: cmd.minLength + chunk.size});
			} else {
				return cmd;
			}
		},
		A2(
			$elm_explorations$test$Simplify$Cmd$chunkCmds,
			$elm_explorations$test$Simplify$Cmd$SwapChunkWithNeighbour,
			{allowChunksOfSize1: false, length: length}));
};
var $elm_explorations$test$Simplify$Cmd$ReplaceChunkWithZero = function (a) {
	return {$: 'ReplaceChunkWithZero', a: a};
};
var $elm_explorations$test$Simplify$Cmd$zeroCmds = function (length) {
	return A2(
		$elm_explorations$test$Simplify$Cmd$chunkCmds,
		$elm_explorations$test$Simplify$Cmd$ReplaceChunkWithZero,
		{allowChunksOfSize1: false, length: length});
};
var $elm_explorations$test$Simplify$Cmd$cmdsForRun = function (run) {
	var length = $elm_explorations$test$RandomRun$length(run);
	return $elm_explorations$test$MicroListExtra$fastConcat(
		_List_fromArray(
			[
				$elm_explorations$test$Simplify$Cmd$deletionCmds(length),
				$elm_explorations$test$Simplify$Cmd$zeroCmds(length),
				A2($elm_explorations$test$Simplify$Cmd$minimizeChoiceCmds, run, length),
				A2($elm_explorations$test$Simplify$Cmd$minimizeFloatCmds, run, length),
				$elm_explorations$test$Simplify$Cmd$sortCmds(length),
				$elm_explorations$test$Simplify$Cmd$redistributeCmds(length),
				$elm_explorations$test$Simplify$Cmd$decrementTogetherCmds(length),
				$elm_explorations$test$Simplify$Cmd$swapCmds(length)
			]));
};
var $elm_explorations$test$RandomRun$sortKey = function (run) {
	return _Utils_Tuple2(
		run.length,
		$elm_explorations$test$RandomRun$toList(run));
};
var $elm_explorations$test$RandomRun$compare = F2(
	function (a, b) {
		return A2(
			$elm$core$Basics$compare,
			$elm_explorations$test$RandomRun$sortKey(a),
			$elm_explorations$test$RandomRun$sortKey(b));
	});
var $elm_explorations$test$Simplify$noImprovement = function (state) {
	return {newState: state, wasImprovement: false};
};
var $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts = false;
var $elm_explorations$test$Simplify$keepIfBetter = F2(
	function (newRandomRun, state) {
		if (A2($elm_explorations$test$RandomRun$equal, state.randomRun, newRandomRun)) {
			return $elm_explorations$test$Simplify$noImprovement(state);
		} else {
			var _v0 = $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts ? A2($elm_explorations$test$Simplify$logRun, 'trying to parse', newRandomRun) : newRandomRun;
			var _v1 = A2(
				$elm_explorations$test$Fuzz$Internal$generate,
				$elm_explorations$test$PRNG$hardcoded(newRandomRun),
				state.fuzzer);
			if (_v1.$ === 'Generated') {
				var value = _v1.a.value;
				var _v2 = state.getExpectation(value);
				if (_v2.$ === 'Pass') {
					var _v3 = $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts ? A2($elm$core$Debug$log, 'parsed but didn\'t fail the test', value) : value;
					return $elm_explorations$test$Simplify$noImprovement(state);
				} else {
					var fail = _v2.a;
					if (_Utils_eq(
						A2($elm_explorations$test$RandomRun$compare, state.randomRun, newRandomRun),
						$elm$core$Basics$GT)) {
						var _v4 = $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts ? A2($elm$core$Debug$log, 'parsed, failed, shrunk', value) : value;
						return {
							newState: _Utils_update(
								state,
								{
									expectation: $elm_explorations$test$Test$Expectation$Fail(fail),
									randomRun: newRandomRun,
									value: value
								}),
							wasImprovement: true
						};
					} else {
						var _v5 = $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts ? A2($elm$core$Debug$log, 'parsed, failed, didn\'t shrink', value) : value;
						return $elm_explorations$test$Simplify$noImprovement(state);
					}
				}
			} else {
				return $elm_explorations$test$Simplify$noImprovement(state);
			}
		}
	});
var $elm_explorations$test$RandomRun$get = F2(
	function (index, run) {
		return A2(
			$elm_explorations$test$MicroListExtra$getAt,
			index,
			$elm_explorations$test$Queue$toList(run.data));
	});
var $elm_explorations$test$Queue$fromList = function (list) {
	return A2($elm_explorations$test$Queue$Queue, list, _List_Nil);
};
var $elm_explorations$test$MicroListExtra$setAt = F4(
	function (index, value, length, list) {
		return ((_Utils_cmp(length, index) < 1) || (index < 0)) ? list : _Utils_ap(
			A2($elm$core$List$take, index, list),
			A2(
				$elm$core$List$cons,
				value,
				A2($elm$core$List$drop, index + 1, list)));
	});
var $elm_explorations$test$RandomRun$replaceInList = F3(
	function (values, len, list) {
		return {
			data: $elm_explorations$test$Queue$fromList(
				A3(
					$elm$core$List$foldl,
					F2(
						function (_v0, accList) {
							var index = _v0.a;
							var newValue = _v0.b;
							return A4(
								$elm_explorations$test$MicroListExtra$setAt,
								index,
								A2($elm$core$Basics$max, 0, newValue),
								len,
								accList);
						}),
					list,
					values)),
			length: len
		};
	});
var $elm_explorations$test$RandomRun$replace = F2(
	function (values, run) {
		return A3(
			$elm_explorations$test$RandomRun$replaceInList,
			values,
			run.length,
			$elm_explorations$test$Queue$toList(run.data));
	});
var $elm_explorations$test$RandomRun$update = F3(
	function (index, fn, run) {
		var _v0 = A2($elm_explorations$test$RandomRun$get, index, run);
		if (_v0.$ === 'Nothing') {
			return run;
		} else {
			var value = _v0.a;
			return A2(
				$elm_explorations$test$RandomRun$replace,
				_List_fromArray(
					[
						_Utils_Tuple2(
						index,
						fn(value))
					]),
				run);
		}
	});
var $elm_explorations$test$Simplify$decrementTogether = F2(
	function (_v0, state) {
		var leftIndex = _v0.leftIndex;
		var rightIndex = _v0.rightIndex;
		var by = _v0.by;
		var simplifiedRun = A3(
			$elm_explorations$test$RandomRun$update,
			rightIndex,
			function (n) {
				return n - by;
			},
			A3(
				$elm_explorations$test$RandomRun$update,
				leftIndex,
				function (n) {
					return n - by;
				},
				state.randomRun));
		return A2($elm_explorations$test$Simplify$keepIfBetter, simplifiedRun, state);
	});
var $elm_explorations$test$RandomRun$isInBounds = F2(
	function (_v0, run) {
		var startIndex = _v0.startIndex;
		var size = _v0.size;
		return _Utils_cmp(startIndex + size, run.length) < 1;
	});
var $elm_explorations$test$RandomRun$deleteChunk = F2(
	function (chunk, run) {
		if (A2($elm_explorations$test$RandomRun$isInBounds, chunk, run)) {
			var list = $elm_explorations$test$Queue$toList(run.data);
			var result = _Utils_update(
				run,
				{
					data: $elm_explorations$test$Queue$fromList(
						_Utils_ap(
							A2($elm$core$List$take, chunk.startIndex, list),
							A2($elm$core$List$drop, chunk.startIndex + chunk.size, list))),
					length: run.length - chunk.size
				});
			return result;
		} else {
			return run;
		}
	});
var $elm_explorations$test$Simplify$deleteChunkAndMaybeDecrementPrevious = F2(
	function (chunk, state) {
		var runWithDelete = A2($elm_explorations$test$RandomRun$deleteChunk, chunk, state.randomRun);
		var runWithDeleteAndDecrement = A3(
			$elm_explorations$test$RandomRun$update,
			chunk.startIndex - 1,
			function (x) {
				return x - 1;
			},
			runWithDelete);
		var afterDeleteAndDecrement = A2($elm_explorations$test$Simplify$keepIfBetter, runWithDeleteAndDecrement, state);
		return afterDeleteAndDecrement.wasImprovement ? afterDeleteAndDecrement : A2($elm_explorations$test$Simplify$keepIfBetter, runWithDelete, state);
	});
var $elm_explorations$test$Simplify$binarySearchLoop = F2(
	function (old, options) {
		binarySearchLoop:
		while (true) {
			var low = options.low;
			var high = options.high;
			var state = options.state;
			var updateRun = options.updateRun;
			if (_Utils_cmp(low + 1, high) < 0) {
				var mid = low + $elm$core$Basics$round((high - low) / 2);
				var newRun = A2(updateRun, mid, options.state.randomRun);
				var afterMid = A2($elm_explorations$test$Simplify$keepIfBetter, newRun, state);
				var optionsWithNewRange = afterMid.wasImprovement ? _Utils_update(
					options,
					{high: mid}) : _Utils_update(
					options,
					{low: mid});
				var newOptions = _Utils_update(
					optionsWithNewRange,
					{state: afterMid.newState});
				var $temp$old = {wasImprovement: afterMid.wasImprovement},
					$temp$options = newOptions;
				old = $temp$old;
				options = $temp$options;
				continue binarySearchLoop;
			} else {
				return {newState: options.state, wasImprovement: old.wasImprovement};
			}
		}
	});
var $elm_explorations$test$Simplify$binarySearchShrink = function (options) {
	var updateRun = options.updateRun;
	var low = options.low;
	var state = options.state;
	var runWithLow = A2(updateRun, low, options.state.randomRun);
	var afterLow = A2($elm_explorations$test$Simplify$keepIfBetter, runWithLow, state);
	return afterLow.wasImprovement ? afterLow : A2(
		$elm_explorations$test$Simplify$binarySearchLoop,
		{wasImprovement: false},
		options);
};
var $elm_explorations$test$RandomRun$set = F3(
	function (index, value, run) {
		return (_Utils_cmp(run.length, index) < 1) ? run : _Utils_update(
			run,
			{
				data: $elm_explorations$test$Queue$fromList(
					A4(
						$elm_explorations$test$MicroListExtra$setAt,
						index,
						A2($elm$core$Basics$max, 0, value),
						run.length,
						$elm_explorations$test$Queue$toList(run.data)))
			});
	});
var $elm_explorations$test$Simplify$minimizeChoice = F2(
	function (_v0, state) {
		var index = _v0.index;
		var _v1 = A2($elm_explorations$test$RandomRun$get, index, state.randomRun);
		if (_v1.$ === 'Nothing') {
			return $elm_explorations$test$Simplify$noImprovement(state);
		} else {
			var value = _v1.a;
			return (!value) ? $elm_explorations$test$Simplify$noImprovement(state) : $elm_explorations$test$Simplify$binarySearchShrink(
				{
					high: value,
					low: 0,
					state: state,
					updateRun: F2(
						function (value_, accRun) {
							return A3($elm_explorations$test$RandomRun$set, index, value_, accRun);
						})
				});
		}
	});
var $elm_explorations$test$Simplify$andThen = F2(
	function (fn, _v0) {
		var newState = _v0.newState;
		return fn(newState);
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm_explorations$test$MicroBitwiseExtra$ones = function (count) {
	return (count === 32) ? 4294967295 : ((1 << count) - 1);
};
var $elm_explorations$test$MicroBitwiseExtra$keepBits = F2(
	function (count, num) {
		return $elm_explorations$test$MicroBitwiseExtra$ones(count) & num;
	});
var $elm_explorations$test$Fuzz$Float$getExponent = function (_v0) {
	var hi = _v0.a;
	return A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 11, hi >>> 20);
};
var $elm_explorations$test$Fuzz$Float$getMantissaTuple = function (_v0) {
	var hi = _v0.a;
	var lo = _v0.b;
	return _Utils_Tuple2(
		A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 20, hi),
		A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 32, lo));
};
var $elm_explorations$test$MicroBitwiseExtra$signedToUnsigned = $elm$core$Bitwise$shiftRightZfBy(0);
var $elm_explorations$test$MicroBitwiseExtra$int52FromTuple = function (_v0) {
	var highBits = _v0.a;
	var lowBits = _v0.b;
	return (4294967296 * $elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(
		A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 20, highBits))) + A2(
		$elm_explorations$test$MicroBitwiseExtra$keepBits,
		32,
		$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(lowBits));
};
var $elm_explorations$test$Fuzz$Float$getMantissa = function (_v0) {
	var hi = _v0.a;
	var lo = _v0.b;
	return $elm_explorations$test$MicroBitwiseExtra$int52FromTuple(
		$elm_explorations$test$Fuzz$Float$getMantissaTuple(
			_Utils_Tuple2(hi, lo)));
};
var $elm_explorations$test$MicroBitwiseExtra$isBitSet = F2(
	function (index, num) {
		isBitSet:
		while (true) {
			if (index >= 32) {
				var $temp$index = index - 32,
					$temp$num = (num / 4294967295) | 0;
				index = $temp$index;
				num = $temp$num;
				continue isBitSet;
			} else {
				return (1 & (num >>> index)) === 1;
			}
		}
	});
var $elm_explorations$test$Fuzz$Float$isFractional = function (hi) {
	return A2($elm_explorations$test$MicroBitwiseExtra$isBitSet, 31, hi);
};
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm_explorations$test$Fuzz$Float$setExponent = F2(
	function (exponent, _v0) {
		var hi = _v0.a;
		var lo = _v0.b;
		return _Utils_Tuple2(
			$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(
				(A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 11, exponent) << 20) | (2148532223 & hi)),
			lo);
	});
var $elm_explorations$test$MicroBitwiseExtra$int52ToTuple = function (n) {
	return _Utils_Tuple2(
		$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(
			A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 20, (n / 4294967296) | 0)),
		$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(
			A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 32, n)));
};
var $elm_explorations$test$Fuzz$Float$setMantissa = F2(
	function (mantissa, _v0) {
		var hi = _v0.a;
		var _v1 = $elm_explorations$test$MicroBitwiseExtra$int52ToTuple(mantissa);
		var mantissaHi = _v1.a;
		var mantissaLo = _v1.b;
		return _Utils_Tuple2(
			$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(
				A2($elm_explorations$test$MicroBitwiseExtra$keepBits, 20, mantissaHi) | (4293918720 & hi)),
			$elm_explorations$test$MicroBitwiseExtra$signedToUnsigned(mantissaLo));
	});
var $elm_explorations$test$Simplify$minimizeFloat = F2(
	function (_v0, state) {
		var leftIndex = _v0.leftIndex;
		var _v1 = A2($elm_explorations$test$RandomRun$get, leftIndex, state.randomRun);
		if (_v1.$ === 'Nothing') {
			return $elm_explorations$test$Simplify$noImprovement(state);
		} else {
			var hi_ = _v1.a;
			if ($elm_explorations$test$Fuzz$Float$isFractional(hi_)) {
				var minimizeMantissaPart = function (state_) {
					var _v5 = A3(
						$elm$core$Maybe$map2,
						$elm$core$Tuple$pair,
						A2($elm_explorations$test$RandomRun$get, leftIndex, state_.randomRun),
						A2($elm_explorations$test$RandomRun$get, leftIndex + 1, state_.randomRun));
					if (_v5.$ === 'Nothing') {
						return $elm_explorations$test$Simplify$noImprovement(state_);
					} else {
						var _v6 = _v5.a;
						var hi = _v6.a;
						var lo = _v6.b;
						var mantissa = $elm_explorations$test$Fuzz$Float$getMantissa(
							_Utils_Tuple2(hi, lo));
						return $elm_explorations$test$Simplify$binarySearchShrink(
							{
								high: mantissa,
								low: 0,
								state: state_,
								updateRun: F2(
									function (newMantissa, accRun) {
										var _v7 = A2(
											$elm_explorations$test$Fuzz$Float$setMantissa,
											newMantissa,
											_Utils_Tuple2(hi, lo));
										var newHi = _v7.a;
										var newLo = _v7.b;
										return A3(
											$elm_explorations$test$RandomRun$set,
											leftIndex + 1,
											newLo,
											A3($elm_explorations$test$RandomRun$set, leftIndex, newHi, accRun));
									})
							});
					}
				};
				var minimizeExponentPart = function (state_) {
					var _v2 = A3(
						$elm$core$Maybe$map2,
						$elm$core$Tuple$pair,
						A2($elm_explorations$test$RandomRun$get, leftIndex, state_.randomRun),
						A2($elm_explorations$test$RandomRun$get, leftIndex + 1, state_.randomRun));
					if (_v2.$ === 'Nothing') {
						return $elm_explorations$test$Simplify$noImprovement(state_);
					} else {
						var _v3 = _v2.a;
						var hi = _v3.a;
						var lo = _v3.b;
						var exponent = $elm_explorations$test$Fuzz$Float$getExponent(
							_Utils_Tuple2(hi, lo));
						return $elm_explorations$test$Simplify$binarySearchShrink(
							{
								high: exponent,
								low: 0,
								state: state_,
								updateRun: F2(
									function (newExponent, accRun) {
										var _v4 = A2(
											$elm_explorations$test$Fuzz$Float$setExponent,
											newExponent,
											_Utils_Tuple2(hi, lo));
										var newHi = _v4.a;
										var newLo = _v4.b;
										return A3(
											$elm_explorations$test$RandomRun$set,
											leftIndex + 1,
											newLo,
											A3($elm_explorations$test$RandomRun$set, leftIndex, newHi, accRun));
									})
							});
					}
				};
				return A2(
					$elm_explorations$test$Simplify$andThen,
					minimizeMantissaPart,
					minimizeExponentPart(state));
			} else {
				return $elm_explorations$test$Simplify$noImprovement(state);
			}
		}
	});
var $elm_explorations$test$RandomRun$swapIfOutOfOrder = F2(
	function (_v0, run) {
		var leftIndex = _v0.leftIndex;
		var rightIndex = _v0.rightIndex;
		var list = $elm_explorations$test$Queue$toList(run.data);
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (left, right) {
					return (_Utils_cmp(left, right) > 0) ? {
						newLeftValue: right,
						newRightValue: left,
						newRun: A3(
							$elm_explorations$test$RandomRun$replaceInList,
							_List_fromArray(
								[
									_Utils_Tuple2(leftIndex, right),
									_Utils_Tuple2(rightIndex, left)
								]),
							run.length,
							list)
					} : {newLeftValue: left, newRightValue: right, newRun: run};
				}),
			A2($elm_explorations$test$MicroListExtra$getAt, leftIndex, list),
			A2($elm_explorations$test$MicroListExtra$getAt, rightIndex, list));
	});
var $elm_explorations$test$Simplify$redistributeChoicesAndMaybeIncrement = F2(
	function (options, state) {
		var _v0 = A2($elm_explorations$test$RandomRun$swapIfOutOfOrder, options, state.randomRun);
		if (_v0.$ === 'Nothing') {
			return $elm_explorations$test$Simplify$noImprovement(state);
		} else {
			var newRun = _v0.a.newRun;
			var newLeftValue = _v0.a.newLeftValue;
			var newRightValue = _v0.a.newRightValue;
			var afterSwap = A2($elm_explorations$test$Simplify$keepIfBetter, newRun, state);
			var newState = afterSwap.newState;
			var go = function (initialRun) {
				return $elm_explorations$test$Simplify$binarySearchShrink(
					{
						high: newLeftValue,
						low: 0,
						state: _Utils_update(
							newState,
							{randomRun: initialRun}),
						updateRun: F2(
							function (value, accRun) {
								return A2(
									$elm_explorations$test$RandomRun$replace,
									_List_fromArray(
										[
											_Utils_Tuple2(options.leftIndex, value),
											_Utils_Tuple2(options.rightIndex, (newRightValue + newLeftValue) - value)
										]),
									accRun);
							})
					});
			};
			var afterShrinkAlone = A2(
				$elm_explorations$test$Simplify$keepIfBetter,
				go(newState.randomRun).newState.randomRun,
				newState);
			if (afterShrinkAlone.wasImprovement) {
				return afterShrinkAlone;
			} else {
				var runWithIncrementedRightBucket = A3(
					$elm_explorations$test$RandomRun$update,
					options.rightIndex - 1,
					function (x) {
						return x + 1;
					},
					newState.randomRun);
				var afterIncrementAndShrink = A2(
					$elm_explorations$test$Simplify$keepIfBetter,
					go(runWithIncrementedRightBucket).newState.randomRun,
					newState);
				return afterIncrementAndShrink.wasImprovement ? afterIncrementAndShrink : afterSwap;
			}
		}
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm_explorations$test$RandomRun$replaceChunkWithZero = F2(
	function (chunk, run) {
		if (A2($elm_explorations$test$RandomRun$isInBounds, chunk, run)) {
			var list = $elm_explorations$test$Queue$toList(run.data);
			return _Utils_update(
				run,
				{
					data: $elm_explorations$test$Queue$fromList(
						$elm_explorations$test$MicroListExtra$fastConcat(
							_List_fromArray(
								[
									A2($elm$core$List$take, chunk.startIndex, list),
									A2($elm$core$List$repeat, chunk.size, 0),
									A2($elm$core$List$drop, chunk.startIndex + chunk.size, list)
								])))
				});
		} else {
			return run;
		}
	});
var $elm_explorations$test$Simplify$replaceChunkWithZero = F2(
	function (chunk, state) {
		var simplifiedRun = A2($elm_explorations$test$RandomRun$replaceChunkWithZero, chunk, state.randomRun);
		return A2($elm_explorations$test$Simplify$keepIfBetter, simplifiedRun, state);
	});
var $elm_explorations$test$RandomRun$getChunk = F2(
	function (chunk, run) {
		return A2($elm_explorations$test$RandomRun$isInBounds, chunk, run) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$List$take,
				chunk.size,
				A2(
					$elm$core$List$drop,
					chunk.startIndex,
					$elm_explorations$test$Queue$toList(run.data)))) : $elm$core$Maybe$Nothing;
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $elm_explorations$test$RandomRun$sortChunk = F2(
	function (chunk, run) {
		var _v0 = A2($elm_explorations$test$RandomRun$getChunk, chunk, run);
		if (_v0.$ === 'Nothing') {
			return run;
		} else {
			var chunkData = _v0.a;
			var sortedIndexed = A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, value) {
						return _Utils_Tuple2(chunk.startIndex + i, value);
					}),
				$elm$core$List$sort(chunkData));
			return A2($elm_explorations$test$RandomRun$replace, sortedIndexed, run);
		}
	});
var $elm_explorations$test$Simplify$sortChunk = F2(
	function (chunk, state) {
		var simplifiedRun = A2($elm_explorations$test$RandomRun$sortChunk, chunk, state.randomRun);
		return A2($elm_explorations$test$Simplify$keepIfBetter, simplifiedRun, state);
	});
var $elm_explorations$test$RandomRun$swapChunks = F2(
	function (_v0, run) {
		var leftChunk = _v0.leftChunk;
		var rightChunk = _v0.rightChunk;
		var list = $elm_explorations$test$Queue$toList(run.data);
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (lefts, rights) {
					return A3(
						$elm_explorations$test$RandomRun$replaceInList,
						$elm$core$List$concat(
							_List_fromArray(
								[
									A2(
									$elm$core$List$indexedMap,
									F2(
										function (i, n) {
											return _Utils_Tuple2(rightChunk.startIndex + i, n);
										}),
									lefts),
									A2(
									$elm$core$List$indexedMap,
									F2(
										function (i, n) {
											return _Utils_Tuple2(leftChunk.startIndex + i, n);
										}),
									rights)
								])),
						run.length,
						list);
				}),
			A2($elm_explorations$test$RandomRun$getChunk, leftChunk, run),
			A2($elm_explorations$test$RandomRun$getChunk, rightChunk, run));
	});
var $elm_explorations$test$Simplify$swapChunkWithNeighbour = F2(
	function (chunk, state) {
		var otherChunk = {size: chunk.size, startIndex: chunk.startIndex + chunk.size};
		return A2(
			$elm$core$Maybe$withDefault,
			$elm_explorations$test$Simplify$noImprovement(state),
			A2(
				$elm$core$Maybe$map,
				function (simplifiedRun) {
					return A2($elm_explorations$test$Simplify$keepIfBetter, simplifiedRun, state);
				},
				A2(
					$elm_explorations$test$RandomRun$swapChunks,
					{leftChunk: chunk, rightChunk: otherChunk},
					state.randomRun)));
	});
var $elm_explorations$test$Simplify$runCmd = F2(
	function (cmd, state) {
		var _v0 = $elm_explorations$test$DebugConfig$shouldLogShrinkAttempts ? A2(
			$elm_explorations$test$Simplify$logRun,
			'trying ' + ($elm$core$Debug$toString(cmd.type_) + ' on'),
			state.randomRun) : state.randomRun;
		var result = function () {
			var _v1 = cmd.type_;
			switch (_v1.$) {
				case 'DeleteChunkAndMaybeDecrementPrevious':
					var chunk = _v1.a;
					return A2($elm_explorations$test$Simplify$deleteChunkAndMaybeDecrementPrevious, chunk, state);
				case 'ReplaceChunkWithZero':
					var chunk = _v1.a;
					return A2($elm_explorations$test$Simplify$replaceChunkWithZero, chunk, state);
				case 'SortChunk':
					var chunk = _v1.a;
					return A2($elm_explorations$test$Simplify$sortChunk, chunk, state);
				case 'MinimizeFloat':
					var options = _v1.a;
					return A2($elm_explorations$test$Simplify$minimizeFloat, options, state);
				case 'MinimizeChoice':
					var options = _v1.a;
					return A2($elm_explorations$test$Simplify$minimizeChoice, options, state);
				case 'RedistributeChoicesAndMaybeIncrement':
					var options = _v1.a;
					return A2($elm_explorations$test$Simplify$redistributeChoicesAndMaybeIncrement, options, state);
				case 'DecrementTogether':
					var options = _v1.a;
					return A2($elm_explorations$test$Simplify$decrementTogether, options, state);
				default:
					var chunk = _v1.a;
					return A2($elm_explorations$test$Simplify$swapChunkWithNeighbour, chunk, state);
			}
		}();
		return result;
	});
var $elm_explorations$test$Simplify$runCmds = F2(
	function (cmds, state) {
		runCmds:
		while (true) {
			if (!cmds.b) {
				return state;
			} else {
				var cmd = cmds.a;
				var rest = cmds.b;
				var _v1 = A2($elm_explorations$test$Simplify$runCmd, cmd, state);
				var wasImprovement = _v1.wasImprovement;
				var newState = _v1.newState;
				var newLength = $elm_explorations$test$RandomRun$length(newState.randomRun);
				var newRest = (wasImprovement && (_Utils_cmp(
					newLength,
					$elm_explorations$test$RandomRun$length(state.randomRun)) < 0)) ? A2(
					$elm$core$List$filter,
					function (_v2) {
						var minLength = _v2.minLength;
						return _Utils_cmp(newLength, minLength) > -1;
					},
					rest) : rest;
				var $temp$cmds = newRest,
					$temp$state = newState;
				cmds = $temp$cmds;
				state = $temp$state;
				continue runCmds;
			}
		}
	});
var $elm_explorations$test$Simplify$simplifyOnce = function (state) {
	return A2(
		$elm_explorations$test$Simplify$runCmds,
		$elm_explorations$test$Simplify$Cmd$cmdsForRun(state.randomRun),
		state);
};
var $elm_explorations$test$Simplify$simplifyWhileProgress = function (state) {
	simplifyWhileProgress:
	while (true) {
		var nextState = $elm_explorations$test$Simplify$simplifyOnce(state);
		if (A2($elm_explorations$test$RandomRun$equal, nextState.randomRun, state.randomRun)) {
			var _v0 = $elm_explorations$test$DebugConfig$shouldLogShrinkProgress ? A2($elm_explorations$test$Simplify$logState, 'shrank successfully', state) : state;
			return _Utils_Tuple3(nextState.value, nextState.randomRun, nextState.expectation);
		} else {
			var $temp$state = nextState;
			state = $temp$state;
			continue simplifyWhileProgress;
		}
	}
};
var $elm_explorations$test$Simplify$simplify = function (state) {
	var _v0 = $elm_explorations$test$DebugConfig$shouldLogFirstFailure ? A2($elm_explorations$test$Simplify$logRun, 'Found failure with RandomRun', state.randomRun) : state.randomRun;
	return $elm_explorations$test$RandomRun$isEmpty(state.randomRun) ? _Utils_Tuple3(state.value, state.randomRun, state.expectation) : $elm_explorations$test$Simplify$simplifyWhileProgress(state);
};
var $elm_explorations$test$Test$Fuzz$findSimplestFailure = function (state) {
	var _v0 = $elm_explorations$test$Simplify$simplify(state);
	var simplestValue = _v0.a;
	var expectation = _v0.c;
	return {
		expectation: expectation,
		given: $elm$core$Maybe$Just(
			$elm_explorations$test$Test$Internal$toString(simplestValue))
	};
};
var $elm_explorations$test$Test$Fuzz$testGeneratedValue = function (state) {
	var _v0 = state.expectation;
	if (_v0.$ === 'Pass') {
		return $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Just(
			$elm_explorations$test$Test$Fuzz$findSimplestFailure(state));
	}
};
var $elm_explorations$test$Test$Fuzz$runOnce = F2(
	function (c, state) {
		var genResult = A2(
			$elm_explorations$test$Fuzz$Internal$generate,
			$elm_explorations$test$PRNG$random(state.currentSeed),
			c.fuzzer);
		var maybeNextSeed = $elm_explorations$test$PRNG$getSeed(
			$elm_explorations$test$GenResult$getPrng(genResult));
		var nextSeed = function () {
			if (maybeNextSeed.$ === 'Just') {
				var seed = maybeNextSeed.a;
				return seed;
			} else {
				return $elm_explorations$test$Test$Fuzz$stepSeed(state.currentSeed);
			}
		}();
		var _v0 = function () {
			if (genResult.$ === 'Rejected') {
				var reason = genResult.a.reason;
				return _Utils_Tuple2(
					$elm$core$Maybe$Just(
						{
							expectation: $elm_explorations$test$Test$Expectation$fail(
								{
									description: reason,
									reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$InvalidFuzzer)
								}),
							given: $elm$core$Maybe$Nothing
						}),
					state.distributionCount);
			} else {
				var prng = genResult.a.prng;
				var value = genResult.a.value;
				var failure = $elm_explorations$test$Test$Fuzz$testGeneratedValue(
					{
						expectation: c.testFn(value),
						fuzzer: c.fuzzer,
						getExpectation: c.testFn,
						randomRun: $elm_explorations$test$PRNG$getRun(prng),
						value: value
					});
				var distributionCounter = A3(
					$elm$core$Maybe$map2,
					F2(
						function (labels, old) {
							var foundLabels = A2(
								$elm$core$List$filterMap,
								function (_v2) {
									var label = _v2.a;
									var predicate = _v2.b;
									return predicate(value) ? $elm$core$Maybe$Just(label) : $elm$core$Maybe$Nothing;
								},
								labels);
							return A2($elm_explorations$test$MicroDictExtra$increment, foundLabels, old);
						}),
					$elm_explorations$test$Test$Distribution$Internal$getDistributionLabels(c.distribution),
					state.distributionCount);
				return _Utils_Tuple2(failure, distributionCounter);
			}
		}();
		var maybeFailure = _v0.a;
		var newDistributionCounter = _v0.b;
		return _Utils_update(
			state,
			{currentSeed: nextSeed, distributionCount: newDistributionCounter, failure: maybeFailure, runsElapsed: state.runsElapsed + 1});
	});
var $elm_explorations$test$Test$Fuzz$runNTimes = F3(
	function (times, c, state) {
		runNTimes:
		while (true) {
			if ((times <= 0) || (!_Utils_eq(state.failure, $elm$core$Maybe$Nothing))) {
				return state;
			} else {
				var $temp$times = times - 1,
					$temp$c = c,
					$temp$state = A2($elm_explorations$test$Test$Fuzz$runOnce, c, state);
				times = $temp$times;
				c = $temp$c;
				state = $temp$state;
				continue runNTimes;
			}
		}
	});
var $elm_explorations$test$Test$Fuzz$fuzzLoop = F2(
	function (c, state) {
		fuzzLoop:
		while (true) {
			var _v0 = state.failure;
			if (_v0.$ === 'Just') {
				var failure = _v0.a;
				return {
					distributionReport: function () {
						var _v1 = state.distributionCount;
						if (_v1.$ === 'Nothing') {
							return $elm_explorations$test$Test$Distribution$NoDistribution;
						} else {
							var distributionCount = _v1.a;
							return $elm_explorations$test$Test$Distribution$DistributionToReport(
								{
									distributionCount: $elm_explorations$test$Test$Fuzz$includeCombinationsInBaseCounts(distributionCount),
									runsElapsed: state.runsElapsed
								});
						}
					}(),
					failure: $elm$core$Maybe$Just(failure)
				};
			} else {
				if (_Utils_cmp(state.runsElapsed, c.runsNeeded) < 0) {
					var newState = A3($elm_explorations$test$Test$Fuzz$runNTimes, c.runsNeeded - state.runsElapsed, c, state);
					var $temp$c = c,
						$temp$state = newState;
					c = $temp$c;
					state = $temp$state;
					continue fuzzLoop;
				} else {
					var _v2 = c.distribution;
					switch (_v2.$) {
						case 'NoDistributionNeeded':
							return {distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution, failure: $elm$core$Maybe$Nothing};
						case 'ReportDistribution':
							var _v3 = state.distributionCount;
							if (_v3.$ === 'Nothing') {
								return $elm_explorations$test$Test$Fuzz$distributionBugRunResult;
							} else {
								var distributionCount = _v3.a;
								return {
									distributionReport: $elm_explorations$test$Test$Distribution$DistributionToReport(
										{
											distributionCount: $elm_explorations$test$Test$Fuzz$includeCombinationsInBaseCounts(distributionCount),
											runsElapsed: state.runsElapsed
										}),
									failure: $elm$core$Maybe$Nothing
								};
							}
						default:
							var normalizedDistributionCount = A2($elm$core$Maybe$map, $elm_explorations$test$Test$Fuzz$includeCombinationsInBaseCounts, state.distributionCount);
							if (A3($elm_explorations$test$Test$Fuzz$allSufficientlyCovered, c, state, normalizedDistributionCount)) {
								var _v4 = A3($elm_explorations$test$Test$Fuzz$findBadZeroRelatedCase, c, state, normalizedDistributionCount);
								if (_v4.$ === 'Nothing') {
									if (normalizedDistributionCount.$ === 'Nothing') {
										return $elm_explorations$test$Test$Fuzz$distributionBugRunResult;
									} else {
										var distributionCount = normalizedDistributionCount.a;
										return {
											distributionReport: $elm_explorations$test$Test$Distribution$DistributionCheckSucceeded(
												{distributionCount: distributionCount, runsElapsed: state.runsElapsed}),
											failure: $elm$core$Maybe$Nothing
										};
									}
								} else {
									var failedLabel = _v4.a;
									return A2($elm_explorations$test$Test$Fuzz$distributionFailRunResult, normalizedDistributionCount, failedLabel);
								}
							} else {
								var _v6 = A3($elm_explorations$test$Test$Fuzz$findInsufficientlyCoveredLabel, c, state, normalizedDistributionCount);
								if (_v6.$ === 'Nothing') {
									var newState = A3(
										$elm_explorations$test$Test$Fuzz$runNTimes,
										A2($elm$core$Basics$pow, 2, state.nextPowerOfTwo),
										c,
										state);
									var $temp$c = c,
										$temp$state = _Utils_update(
										newState,
										{nextPowerOfTwo: newState.nextPowerOfTwo + 1});
									c = $temp$c;
									state = $temp$state;
									continue fuzzLoop;
								} else {
									var failedLabel = _v6.a;
									return A2($elm_explorations$test$Test$Fuzz$distributionFailRunResult, normalizedDistributionCount, failedLabel);
								}
							}
					}
				}
			}
		}
	});
var $elm_explorations$test$Test$Fuzz$initLoopState = F2(
	function (initialSeed, distribution) {
		var initialDistributionCount = A2(
			$elm$core$Maybe$map,
			function (labels) {
				return $elm$core$Dict$fromList(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var label = _v0.a;
							return _Utils_Tuple2(
								_List_fromArray(
									[label]),
								0);
						},
						labels));
			},
			$elm_explorations$test$Test$Distribution$Internal$getDistributionLabels(distribution));
		return {currentSeed: initialSeed, distributionCount: initialDistributionCount, failure: $elm$core$Maybe$Nothing, nextPowerOfTwo: 1, runsElapsed: 0};
	});
var $elm_explorations$test$DebugConfig$shouldLogFuzzTests = false;
var $elm_explorations$test$Test$Expectation$withDistributionReport = F2(
	function (newDistributionReport, expectation) {
		if (expectation.$ === 'Fail') {
			var failure = expectation.a;
			return $elm_explorations$test$Test$Expectation$Fail(
				_Utils_update(
					failure,
					{distributionReport: newDistributionReport}));
		} else {
			var pass = expectation.a;
			return $elm_explorations$test$Test$Expectation$Pass(
				_Utils_update(
					pass,
					{distributionReport: newDistributionReport}));
		}
	});
var $elm_explorations$test$Test$Fuzz$validatedFuzzTest = F4(
	function (desc, fuzzer, getExpectation, distribution) {
		return $elm_explorations$test$Test$Internal$ElmTestVariant__FuzzTest(
			F2(
				function (seed, runs) {
					var _v0 = $elm_explorations$test$DebugConfig$shouldLogFuzzTests ? A2($elm$core$Debug$log, 'running fuzz test', desc) : desc;
					var runResult = A2(
						$elm_explorations$test$Test$Fuzz$fuzzLoop,
						{distribution: distribution, fuzzer: fuzzer, initialSeed: seed, runsNeeded: runs, testFn: getExpectation},
						A2($elm_explorations$test$Test$Fuzz$initLoopState, seed, distribution));
					var _v1 = runResult.failure;
					if (_v1.$ === 'Nothing') {
						return _List_fromArray(
							[
								$elm_explorations$test$Test$Expectation$Pass(
								{distributionReport: runResult.distributionReport})
							]);
					} else {
						var failure = _v1.a;
						return _List_fromArray(
							[
								$elm_explorations$test$Test$Fuzz$formatExpectation(
								_Utils_update(
									failure,
									{
										expectation: A2($elm_explorations$test$Test$Expectation$withDistributionReport, runResult.distributionReport, failure.expectation)
									}))
							]);
					}
				}));
	});
var $elm_explorations$test$Test$Fuzz$fuzzTest = F4(
	function (distribution, fuzzer, untrimmedDesc, getExpectation) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		return $elm$core$String$isEmpty(desc) ? $elm_explorations$test$Test$Internal$blankDescriptionFailure : A2(
			$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
			desc,
			A4($elm_explorations$test$Test$Fuzz$validatedFuzzTest, desc, fuzzer, getExpectation, distribution));
	});
var $elm_explorations$test$Test$fuzz = $elm_explorations$test$Test$Fuzz$fuzzTest($elm_explorations$test$Test$Distribution$Internal$NoDistributionNeeded);
var $elm_explorations$test$Test$fuzz2 = F3(
	function (fuzzA, fuzzB, desc) {
		var fuzzer = A2($elm_explorations$test$Fuzz$pair, fuzzA, fuzzB);
		return A2(
			$elm$core$Basics$composeR,
			F2(
				function (f, _v0) {
					var a = _v0.a;
					var b = _v0.b;
					return A2(f, a, b);
				}),
			A2($elm_explorations$test$Test$fuzz, fuzzer, desc));
	});
var $author$project$EffectivenessSUT$Bst$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst$union,
				right,
				A2($author$project$EffectivenessSUT$Bst$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst$union,
				left,
				A2($author$project$EffectivenessSUT$Bst$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessTests$makeBstSuite = F4(
	function (label, insert, _delete, union) {
		return A2(
			$elm_explorations$test$Test$describe,
			label,
			_List_fromArray(
				[
					A4(
					$elm_explorations$test$Test$fuzz2,
					$author$project$EffectivenessTests$bstFuzzer,
					$author$project$EffectivenessTests$pairFuzzer,
					'insert',
					F2(
						function (t, _v0) {
							var k = _v0.a;
							var v = _v0.b;
							return A2(
								$elm_explorations$test$Expect$equal,
								A3($author$project$EffectivenessSUT$Bst$insert, k, v, t),
								A3(insert, k, v, t));
						})),
					A4(
					$elm_explorations$test$Test$fuzz2,
					$author$project$EffectivenessTests$bstFuzzer,
					$author$project$EffectivenessTests$keyFuzzer,
					'delete',
					F2(
						function (t, k) {
							return A2(
								$elm_explorations$test$Expect$equal,
								A2($author$project$EffectivenessSUT$Bst$delete, k, t),
								A2(_delete, k, t));
						})),
					A4(
					$elm_explorations$test$Test$fuzz2,
					$author$project$EffectivenessTests$bstFuzzer,
					$author$project$EffectivenessTests$bstFuzzer,
					'union',
					F2(
						function (t1, t2) {
							return A2(
								$elm_explorations$test$Expect$equal,
								A2($author$project$EffectivenessSUT$Bst$union, t1, t2),
								A2(union, t1, t2));
						}))
				]));
	});
var $author$project$EffectivenessSUT$Bst1$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst1$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst1$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst1$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst1$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst1$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst1$union,
				right,
				A2($author$project$EffectivenessSUT$Bst1$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst1$union,
				left,
				A2($author$project$EffectivenessSUT$Bst1$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessSUT$Bst2$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst2$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst2$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst2$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst2$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst2$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst2$union,
				right,
				A2($author$project$EffectivenessSUT$Bst2$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst2$union,
				left,
				A2($author$project$EffectivenessSUT$Bst2$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessSUT$Bst3$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst3$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst3$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst3$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst3$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst3$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst3$union,
				right,
				A2($author$project$EffectivenessSUT$Bst3$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst3$union,
				left,
				A2($author$project$EffectivenessSUT$Bst3$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessSUT$Bst4$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst4$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst4$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst4$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst4$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst4$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst4$union,
				right,
				A2($author$project$EffectivenessSUT$Bst4$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst4$union,
				left,
				A2($author$project$EffectivenessSUT$Bst4$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessSUT$Bst5$leftSubtreeOf = F2(
	function (k, tree) {
		leftSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) < 1) {
					var $temp$k = k,
						$temp$tree = left;
					k = $temp$k;
					tree = $temp$tree;
					continue leftSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						left,
						key,
						val,
						A2($author$project$EffectivenessSUT$Bst5$leftSubtreeOf, k, right));
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst5$rightSubtreeOf = F2(
	function (k, tree) {
		rightSubtreeOf:
		while (true) {
			if (tree.$ === 'Leaf') {
				return $author$project$EffectivenessSUT$BstCommon$Leaf;
			} else {
				var left = tree.a;
				var key = tree.b;
				var val = tree.c;
				var right = tree.d;
				if (_Utils_cmp(k, key) > -1) {
					var $temp$k = k,
						$temp$tree = right;
					k = $temp$k;
					tree = $temp$tree;
					continue rightSubtreeOf;
				} else {
					return A4(
						$author$project$EffectivenessSUT$BstCommon$Node,
						A2($author$project$EffectivenessSUT$Bst5$rightSubtreeOf, k, left),
						key,
						val,
						right);
				}
			}
		}
	});
var $author$project$EffectivenessSUT$Bst5$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			var left = t1.a;
			var k = t1.b;
			var v = t1.c;
			var right = t1.d;
			var t2Without = A2($author$project$EffectivenessSUT$Bst5$delete, k, t2);
			var newRight = A2(
				$author$project$EffectivenessSUT$Bst5$union,
				right,
				A2($author$project$EffectivenessSUT$Bst5$rightSubtreeOf, k, t2Without));
			var newLeft = A2(
				$author$project$EffectivenessSUT$Bst5$union,
				left,
				A2($author$project$EffectivenessSUT$Bst5$leftSubtreeOf, k, t2Without));
			return A4($author$project$EffectivenessSUT$BstCommon$Node, newLeft, k, v, newRight);
		}
	});
var $author$project$EffectivenessSUT$Bst6$union = F2(
	function (t1, t2) {
		if (t1.$ === 'Leaf') {
			return t2;
		} else {
			if (t1.d.$ === 'Leaf') {
				var left = t1.a;
				var k = t1.b;
				var v = t1.c;
				var _v1 = t1.d;
				return A4($author$project$EffectivenessSUT$BstCommon$Node, left, k, v, t2);
			} else {
				var left = t1.a;
				var k = t1.b;
				var v = t1.c;
				var right = t1.d;
				return A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					left,
					k,
					v,
					A2($author$project$EffectivenessSUT$Bst6$union, right, t2));
			}
		}
	});
var $author$project$EffectivenessSUT$Bst7$union = F2(
	function (t1, t2) {
		var _v0 = _Utils_Tuple2(t1, t2);
		if (_v0.a.$ === 'Leaf') {
			var _v1 = _v0.a;
			return t2;
		} else {
			if (_v0.b.$ === 'Leaf') {
				var _v2 = _v0.b;
				return t1;
			} else {
				var _v3 = _v0.a;
				var l1 = _v3.a;
				var k1 = _v3.b;
				var v1 = _v3.c;
				var r1 = _v3.d;
				var _v4 = _v0.b;
				var l2 = _v4.a;
				var k2 = _v4.b;
				var v2 = _v4.c;
				var r2 = _v4.d;
				return _Utils_eq(k1, k2) ? A4(
					$author$project$EffectivenessSUT$BstCommon$Node,
					A2($author$project$EffectivenessSUT$Bst7$union, l1, l2),
					k1,
					v1,
					A2($author$project$EffectivenessSUT$Bst7$union, r1, r2)) : ((_Utils_cmp(k1, k2) < 0) ? A4($author$project$EffectivenessSUT$BstCommon$Node, t1, k2, v2, r2) : A4($author$project$EffectivenessSUT$BstCommon$Node, l2, k2, v2, t1));
			}
		}
	});
var $author$project$EffectivenessSUT$Bst8$treeToList = function (tree) {
	if (tree.$ === 'Leaf') {
		return _List_Nil;
	} else {
		var left = tree.a;
		var k = tree.b;
		var v = tree.c;
		var right = tree.d;
		return _Utils_ap(
			$author$project$EffectivenessSUT$Bst8$treeToList(left),
			_Utils_ap(
				_List_fromArray(
					[
						_Utils_Tuple2(k, v)
					]),
				$author$project$EffectivenessSUT$Bst8$treeToList(right)));
	}
};
var $author$project$EffectivenessSUT$Bst8$union = F2(
	function (t1, t2) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, acc) {
					var k = _v0.a;
					var v = _v0.b;
					return A3($author$project$EffectivenessSUT$Bst8$insert, k, v, acc);
				}),
			t2,
			$author$project$EffectivenessSUT$Bst8$treeToList(t1));
	});
var $author$project$EffectivenessTests$all = A2(
	$elm_explorations$test$Test$describe,
	'Fuzzer effectiveness benchmark',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$describe,
			'Binary Search Tree',
			_List_fromArray(
				[
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 1', $author$project$EffectivenessSUT$Bst1$insert, $author$project$EffectivenessSUT$Bst1$delete, $author$project$EffectivenessSUT$Bst1$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 2', $author$project$EffectivenessSUT$Bst2$insert, $author$project$EffectivenessSUT$Bst2$delete, $author$project$EffectivenessSUT$Bst2$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 3', $author$project$EffectivenessSUT$Bst3$insert, $author$project$EffectivenessSUT$Bst3$delete, $author$project$EffectivenessSUT$Bst3$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 4', $author$project$EffectivenessSUT$Bst4$insert, $author$project$EffectivenessSUT$Bst4$delete, $author$project$EffectivenessSUT$Bst4$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 5', $author$project$EffectivenessSUT$Bst5$insert, $author$project$EffectivenessSUT$Bst5$delete, $author$project$EffectivenessSUT$Bst5$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 6', $author$project$EffectivenessSUT$Bst6$insert, $author$project$EffectivenessSUT$Bst6$delete, $author$project$EffectivenessSUT$Bst6$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 7', $author$project$EffectivenessSUT$Bst7$insert, $author$project$EffectivenessSUT$Bst7$delete, $author$project$EffectivenessSUT$Bst7$union),
					A4($author$project$EffectivenessTests$makeBstSuite, 'bug 8', $author$project$EffectivenessSUT$Bst8$insert, $author$project$EffectivenessSUT$Bst8$delete, $author$project$EffectivenessSUT$Bst8$union)
				]))
		]));
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm_explorations$test$Test$Runner$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm_explorations$test$Test$Runner$Only = function (a) {
	return {$: 'Only', a: a};
};
var $elm_explorations$test$Test$Runner$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var $elm_explorations$test$Test$Runner$Skipping = function (a) {
	return {$: 'Skipping', a: a};
};
var $elm_explorations$test$Test$Runner$countRunnables = function (runnable) {
	countRunnables:
	while (true) {
		if (runnable.$ === 'Runnable') {
			return 1;
		} else {
			var runner = runnable.b;
			var $temp$runnable = runner;
			runnable = $temp$runnable;
			continue countRunnables;
		}
	}
};
var $elm_explorations$test$Test$Runner$countAllRunnables = A2(
	$elm$core$List$foldl,
	A2($elm$core$Basics$composeR, $elm_explorations$test$Test$Runner$countRunnables, $elm$core$Basics$add),
	0);
var $elm_explorations$test$Test$Runner$Labeled = F2(
	function (a, b) {
		return {$: 'Labeled', a: a, b: b};
	});
var $elm_explorations$test$Test$Runner$Runnable = function (a) {
	return {$: 'Runnable', a: a};
};
var $elm_explorations$test$Test$Runner$Thunk = function (a) {
	return {$: 'Thunk', a: a};
};
var $elm_explorations$test$Test$Runner$emptyDistribution = function (seed) {
	return {all: _List_Nil, only: _List_Nil, seed: seed, skipped: _List_Nil};
};
var $elm_explorations$test$Test$Runner$fnvHash = F2(
	function (a, b) {
		return ((a ^ b) * 16777619) >>> 0;
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm_explorations$test$Test$Runner$fnvHashString = F2(
	function (hash, str) {
		return A3(
			$elm$core$List$foldl,
			$elm_explorations$test$Test$Runner$fnvHash,
			hash,
			A2(
				$elm$core$List$map,
				$elm$core$Char$toCode,
				$elm$core$String$toList(str)));
	});
var $elm_explorations$test$Test$Runner$fnvInit = 2166136261;
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $elm$random$Random$maxInt = 2147483647;
var $elm_explorations$test$Test$Runner$batchDistribute = F4(
	function (hashed, runs, test, prev) {
		var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, prev.seed, test);
		return {
			all: _Utils_ap(prev.all, next.all),
			only: _Utils_ap(prev.only, next.only),
			seed: next.seed,
			skipped: _Utils_ap(prev.skipped, next.skipped)
		};
	});
var $elm_explorations$test$Test$Runner$distributeSeedsHelp = F4(
	function (hashed, runs, seed, test) {
		switch (test.$) {
			case 'ElmTestVariant__UnitTest':
				var aRun = test.a;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v1) {
									return aRun(_Utils_Tuple0);
								}))
						]),
					only: _List_Nil,
					seed: seed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__FuzzTest':
				var aRun = test.a;
				var _v2 = A2($elm$random$Random$step, $elm$random$Random$independentSeed, seed);
				var firstSeed = _v2.a;
				var nextSeed = _v2.b;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v3) {
									return A2(aRun, firstSeed, runs);
								}))
						]),
					only: _List_Nil,
					seed: nextSeed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__Labeled':
				var description = test.a;
				var subTest = test.b;
				if (hashed) {
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, seed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: next.seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				} else {
					var intFromSeed = A2(
						$elm$random$Random$step,
						A2($elm$random$Random$int, 0, $elm$random$Random$maxInt),
						seed).a;
					var hashedSeed = $elm$random$Random$initialSeed(
						A2(
							$elm_explorations$test$Test$Runner$fnvHash,
							intFromSeed,
							A2($elm_explorations$test$Test$Runner$fnvHashString, $elm_explorations$test$Test$Runner$fnvInit, description)));
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, hashedSeed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				}
			case 'ElmTestVariant__Skipped':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return {all: _List_Nil, only: _List_Nil, seed: next.seed, skipped: next.all};
			case 'ElmTestVariant__Only':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return _Utils_update(
					next,
					{only: next.all});
			default:
				var tests = test.a;
				return A3(
					$elm$core$List$foldl,
					A2($elm_explorations$test$Test$Runner$batchDistribute, hashed, runs),
					$elm_explorations$test$Test$Runner$emptyDistribution(seed),
					tests);
		}
	});
var $elm_explorations$test$Test$Runner$distributeSeeds = $elm_explorations$test$Test$Runner$distributeSeedsHelp(false);
var $elm_explorations$test$Test$Runner$runThunk = _Test_runThunk;
var $elm_explorations$test$Test$Runner$run = function (_v0) {
	var fn = _v0.a;
	var _v1 = $elm_explorations$test$Test$Runner$runThunk(fn);
	if (_v1.$ === 'Ok') {
		var test = _v1.a;
		return test;
	} else {
		var message = _v1.a;
		return _List_fromArray(
			[
				$elm_explorations$test$Expect$fail('This test failed because it threw an exception: \"' + (message + '\"'))
			]);
	}
};
var $elm_explorations$test$Test$Runner$fromRunnableTreeHelp = F2(
	function (labels, runner) {
		fromRunnableTreeHelp:
		while (true) {
			if (runner.$ === 'Runnable') {
				var runnable = runner.a;
				return _List_fromArray(
					[
						{
						labels: labels,
						run: function (_v1) {
							return $elm_explorations$test$Test$Runner$run(runnable);
						}
					}
					]);
			} else {
				var label = runner.a;
				var subRunner = runner.b;
				var $temp$labels = A2($elm$core$List$cons, label, labels),
					$temp$runner = subRunner;
				labels = $temp$labels;
				runner = $temp$runner;
				continue fromRunnableTreeHelp;
			}
		}
	});
var $elm_explorations$test$Test$Runner$fromRunnableTree = $elm_explorations$test$Test$Runner$fromRunnableTreeHelp(_List_Nil);
var $elm_explorations$test$Test$Runner$fromTest = F3(
	function (runs, seed, test) {
		if (runs < 1) {
			return $elm_explorations$test$Test$Runner$Invalid(
				'Test runner run count must be at least 1, not ' + $elm$core$String$fromInt(runs));
		} else {
			var distribution = A3($elm_explorations$test$Test$Runner$distributeSeeds, runs, seed, test);
			return $elm$core$List$isEmpty(distribution.only) ? ((!$elm_explorations$test$Test$Runner$countAllRunnables(distribution.skipped)) ? $elm_explorations$test$Test$Runner$Plain(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all)) : $elm_explorations$test$Test$Runner$Skipping(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all))) : $elm_explorations$test$Test$Runner$Only(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.only));
		}
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $author$project$Runner$String$Format$hexInt = function (_int) {
	if (!_int) {
		return '0';
	} else {
		var zeroPad4 = function (n) {
			zeroPad4:
			while (true) {
				if ($elm$core$String$length(n) < 4) {
					var $temp$n = '0' + n;
					n = $temp$n;
					continue zeroPad4;
				} else {
					return n;
				}
			}
		};
		var hexIntInternal = function (i) {
			return (!i) ? '' : _Utils_ap(
				hexIntInternal((i / 16) | 0),
				function () {
					var _v0 = i % 16;
					switch (_v0) {
						case 10:
							return 'a';
						case 11:
							return 'b';
						case 12:
							return 'c';
						case 13:
							return 'd';
						case 14:
							return 'e';
						case 15:
							return 'f';
						default:
							var decimalDigit = _v0;
							return $elm$core$String$fromInt(decimalDigit);
					}
				}());
		};
		return zeroPad4(
			hexIntInternal(_int));
	}
};
var $author$project$Runner$String$Format$escapeUnicodeChars = function (s) {
	var isAsciiChar = function (v) {
		return ((32 <= v) && (v <= 125)) && (!A2(
			$elm$core$List$member,
			v,
			_List_fromArray(
				[94, 96])));
	};
	return A2(
		$elm$core$String$join,
		'',
		A2(
			$elm$core$List$map,
			function (c) {
				return isAsciiChar(c) ? $elm$core$String$fromChar(
					$elm$core$Char$fromCode(c)) : ('\\u{' + ($author$project$Runner$String$Format$hexInt(c) + '}'));
			},
			A2(
				$elm$core$List$map,
				$elm$core$Char$toCode,
				$elm$core$String$toList(s))));
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $jinjor$elm_diff$Diff$Added = function (a) {
	return {$: 'Added', a: a};
};
var $jinjor$elm_diff$Diff$CannotGetA = function (a) {
	return {$: 'CannotGetA', a: a};
};
var $jinjor$elm_diff$Diff$CannotGetB = function (a) {
	return {$: 'CannotGetB', a: a};
};
var $jinjor$elm_diff$Diff$NoChange = function (a) {
	return {$: 'NoChange', a: a};
};
var $jinjor$elm_diff$Diff$Removed = function (a) {
	return {$: 'Removed', a: a};
};
var $jinjor$elm_diff$Diff$UnexpectedPath = F2(
	function (a, b) {
		return {$: 'UnexpectedPath', a: a, b: b};
	});
var $jinjor$elm_diff$Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _v0, path) {
		makeChangesHelp:
		while (true) {
			var x = _v0.a;
			var y = _v0.b;
			if (!path.b) {
				return $elm$core$Result$Ok(changes);
			} else {
				var _v2 = path.a;
				var prevX = _v2.a;
				var prevY = _v2.b;
				var tail = path.b;
				var change = function () {
					if (_Utils_eq(x - 1, prevX) && _Utils_eq(y - 1, prevY)) {
						var _v4 = getA(x);
						if (_v4.$ === 'Just') {
							var a = _v4.a;
							return $elm$core$Result$Ok(
								$jinjor$elm_diff$Diff$NoChange(a));
						} else {
							return $elm$core$Result$Err(
								$jinjor$elm_diff$Diff$CannotGetA(x));
						}
					} else {
						if (_Utils_eq(x, prevX)) {
							var _v5 = getB(y);
							if (_v5.$ === 'Just') {
								var b = _v5.a;
								return $elm$core$Result$Ok(
									$jinjor$elm_diff$Diff$Added(b));
							} else {
								return $elm$core$Result$Err(
									$jinjor$elm_diff$Diff$CannotGetB(y));
							}
						} else {
							if (_Utils_eq(y, prevY)) {
								var _v6 = getA(x);
								if (_v6.$ === 'Just') {
									var a = _v6.a;
									return $elm$core$Result$Ok(
										$jinjor$elm_diff$Diff$Removed(a));
								} else {
									return $elm$core$Result$Err(
										$jinjor$elm_diff$Diff$CannotGetA(x));
								}
							} else {
								return $elm$core$Result$Err(
									A2(
										$jinjor$elm_diff$Diff$UnexpectedPath,
										_Utils_Tuple2(x, y),
										path));
							}
						}
					}
				}();
				if (change.$ === 'Ok') {
					var c = change.a;
					var $temp$changes = A2($elm$core$List$cons, c, changes),
						$temp$getA = getA,
						$temp$getB = getB,
						$temp$_v0 = _Utils_Tuple2(prevX, prevY),
						$temp$path = tail;
					changes = $temp$changes;
					getA = $temp$getA;
					getB = $temp$getB;
					_v0 = $temp$_v0;
					path = $temp$path;
					continue makeChangesHelp;
				} else {
					var e = change.a;
					return $elm$core$Result$Err(e);
				}
			}
		}
	});
var $jinjor$elm_diff$Diff$makeChanges = F3(
	function (getA, getB, path) {
		if (!path.b) {
			return $elm$core$Result$Ok(_List_Nil);
		} else {
			var latest = path.a;
			var tail = path.b;
			return A5($jinjor$elm_diff$Diff$makeChangesHelp, _List_Nil, getA, getB, latest, tail);
		}
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $jinjor$elm_diff$Diff$Continue = function (a) {
	return {$: 'Continue', a: a};
};
var $jinjor$elm_diff$Diff$Found = function (a) {
	return {$: 'Found', a: a};
};
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $jinjor$elm_diff$Diff$step = F4(
	function (snake_, offset, k, v) {
		var fromTop = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k - 1) + offset, v));
		var _v0 = function () {
			var _v2 = _Utils_Tuple2(fromLeft, fromTop);
			if (!_v2.a.b) {
				if (!_v2.b.b) {
					return _Utils_Tuple2(
						_List_Nil,
						_Utils_Tuple2(0, 0));
				} else {
					var _v3 = _v2.b;
					var _v4 = _v3.a;
					var topX = _v4.a;
					var topY = _v4.b;
					return _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			} else {
				if (!_v2.b.b) {
					var _v5 = _v2.a;
					var _v6 = _v5.a;
					var leftX = _v6.a;
					var leftY = _v6.b;
					return _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1));
				} else {
					var _v7 = _v2.a;
					var _v8 = _v7.a;
					var leftX = _v8.a;
					var leftY = _v8.b;
					var _v9 = _v2.b;
					var _v10 = _v9.a;
					var topX = _v10.a;
					var topY = _v10.b;
					return (_Utils_cmp(leftY + 1, topY) > -1) ? _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1)) : _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			}
		}();
		var path = _v0.a;
		var _v1 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		var _v11 = A3(
			snake_,
			x + 1,
			y + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(x, y),
				path));
		var newPath = _v11.a;
		var goal = _v11.b;
		return goal ? $jinjor$elm_diff$Diff$Found(newPath) : $jinjor$elm_diff$Diff$Continue(
			A3($elm$core$Array$set, k + offset, newPath, v));
	});
var $jinjor$elm_diff$Diff$onpLoopK = F4(
	function (snake_, offset, ks, v) {
		onpLoopK:
		while (true) {
			if (!ks.b) {
				return $jinjor$elm_diff$Diff$Continue(v);
			} else {
				var k = ks.a;
				var ks_ = ks.b;
				var _v1 = A4($jinjor$elm_diff$Diff$step, snake_, offset, k, v);
				if (_v1.$ === 'Found') {
					var path = _v1.a;
					return $jinjor$elm_diff$Diff$Found(path);
				} else {
					var v_ = _v1.a;
					var $temp$snake_ = snake_,
						$temp$offset = offset,
						$temp$ks = ks_,
						$temp$v = v_;
					snake_ = $temp$snake_;
					offset = $temp$offset;
					ks = $temp$ks;
					v = $temp$v;
					continue onpLoopK;
				}
			}
		}
	});
var $jinjor$elm_diff$Diff$onpLoopP = F5(
	function (snake_, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (delta > 0) ? _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, delta + p)),
				A2($elm$core$List$range, -p, delta)) : _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, p)),
				A2($elm$core$List$range, (-p) + delta, delta));
			var _v0 = A4($jinjor$elm_diff$Diff$onpLoopK, snake_, offset, ks, v);
			if (_v0.$ === 'Found') {
				var path = _v0.a;
				return path;
			} else {
				var v_ = _v0.a;
				var $temp$snake_ = snake_,
					$temp$delta = delta,
					$temp$offset = offset,
					$temp$p = p + 1,
					$temp$v = v_;
				snake_ = $temp$snake_;
				delta = $temp$delta;
				offset = $temp$offset;
				p = $temp$p;
				v = $temp$v;
				continue onpLoopP;
			}
		}
	});
var $jinjor$elm_diff$Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _v0 = _Utils_Tuple2(
				getA(nextX),
				getB(nextY));
			_v0$2:
			while (true) {
				if (_v0.a.$ === 'Just') {
					if (_v0.b.$ === 'Just') {
						var a = _v0.a.a;
						var b = _v0.b.a;
						if (_Utils_eq(a, b)) {
							var $temp$getA = getA,
								$temp$getB = getB,
								$temp$nextX = nextX + 1,
								$temp$nextY = nextY + 1,
								$temp$path = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(nextX, nextY),
								path);
							getA = $temp$getA;
							getB = $temp$getB;
							nextX = $temp$nextX;
							nextY = $temp$nextY;
							path = $temp$path;
							continue snake;
						} else {
							return _Utils_Tuple2(path, false);
						}
					} else {
						break _v0$2;
					}
				} else {
					if (_v0.b.$ === 'Nothing') {
						var _v1 = _v0.a;
						var _v2 = _v0.b;
						return _Utils_Tuple2(path, true);
					} else {
						break _v0$2;
					}
				}
			}
			return _Utils_Tuple2(path, false);
		}
	});
var $jinjor$elm_diff$Diff$onp = F4(
	function (getA, getB, m, n) {
		var v = A2(
			$elm$core$Array$initialize,
			(m + n) + 1,
			$elm$core$Basics$always(_List_Nil));
		var delta = n - m;
		return A5(
			$jinjor$elm_diff$Diff$onpLoopP,
			A2($jinjor$elm_diff$Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var $jinjor$elm_diff$Diff$testDiff = F2(
	function (a, b) {
		var arrB = $elm$core$Array$fromList(b);
		var getB = function (y) {
			return A2($elm$core$Array$get, y - 1, arrB);
		};
		var n = $elm$core$Array$length(arrB);
		var arrA = $elm$core$Array$fromList(a);
		var getA = function (x) {
			return A2($elm$core$Array$get, x - 1, arrA);
		};
		var m = $elm$core$Array$length(arrA);
		var path = A4($jinjor$elm_diff$Diff$onp, getA, getB, m, n);
		return A3($jinjor$elm_diff$Diff$makeChanges, getA, getB, path);
	});
var $jinjor$elm_diff$Diff$diff = F2(
	function (a, b) {
		var _v0 = A2($jinjor$elm_diff$Diff$testDiff, a, b);
		if (_v0.$ === 'Ok') {
			var changes = _v0.a;
			return changes;
		} else {
			return _List_Nil;
		}
	});
var $author$project$Runner$String$Format$formatActualChange = function (diff) {
	switch (diff.$) {
		case 'Added':
			return _Utils_Tuple2('', '');
		case 'Removed':
			var _char = diff.a;
			return _Utils_Tuple2(
				'▼',
				$elm$core$String$fromChar(_char));
		default:
			var _char = diff.a;
			return _Utils_Tuple2(
				' ',
				$elm$core$String$fromChar(_char));
	}
};
var $author$project$Runner$String$Format$formatExpectedChange = function (diff) {
	switch (diff.$) {
		case 'Added':
			return _Utils_Tuple2('', '');
		case 'Removed':
			var _char = diff.a;
			return _Utils_Tuple2(
				$elm$core$String$fromChar(_char),
				'▲');
		default:
			var _char = diff.a;
			return _Utils_Tuple2(
				$elm$core$String$fromChar(_char),
				' ');
	}
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Runner$String$Format$formatEqualityDiffArrows = F2(
	function (below, above) {
		if (_Utils_cmp(
			$elm$core$String$length(below) * $elm$core$String$length(above),
			300 * 300) > 0) {
			return _Utils_Tuple2(
				_Utils_Tuple2(
					_List_fromArray(
						[below]),
					_List_fromArray(
						[' -- skipping diffing because input is too large'])),
				_Utils_Tuple2(
					_List_fromArray(
						[' -- skipping diffing because input is too large']),
					_List_fromArray(
						[above])));
		} else {
			var _v0 = $elm$core$List$unzip(
				A2(
					$elm$core$List$map,
					$author$project$Runner$String$Format$formatActualChange,
					A2(
						$jinjor$elm_diff$Diff$diff,
						$elm$core$String$toList(above),
						$elm$core$String$toList(below))));
			var diffArrowsAbove = _v0.a;
			var valueAbove = _v0.b;
			var _v1 = $elm$core$List$unzip(
				A2(
					$elm$core$List$map,
					$author$project$Runner$String$Format$formatExpectedChange,
					A2(
						$jinjor$elm_diff$Diff$diff,
						$elm$core$String$toList(below),
						$elm$core$String$toList(above))));
			var valueBelow = _v1.a;
			var diffArrowsBelow = _v1.b;
			return _Utils_Tuple2(
				_Utils_Tuple2(valueBelow, diffArrowsBelow),
				_Utils_Tuple2(diffArrowsAbove, valueAbove));
		}
	});
var $author$project$Runner$String$Format$verticalBar = F3(
	function (comparison, below, above) {
		return A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[above, '╵', '│ |> ' + comparison, '╷', below]));
	});
var $author$project$Runner$String$Format$equalityToString = function (_v0) {
	var operation = _v0.operation;
	var expected = _v0.expected;
	var actual = _v0.actual;
	var combine = function (things) {
		return A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				$elm$core$String$join(''),
				things));
	};
	var _v1 = A2($author$project$Runner$String$Format$formatEqualityDiffArrows, expected, actual);
	var _v2 = _v1.a;
	var valueBelow = _v2.a;
	var diffArrowsBelow = _v2.b;
	var _v3 = _v1.b;
	var diffArrowsAbove = _v3.a;
	var valueAbove = _v3.b;
	var _v4 = A2(
		$author$project$Runner$String$Format$formatEqualityDiffArrows,
		$author$project$Runner$String$Format$escapeUnicodeChars(expected),
		$author$project$Runner$String$Format$escapeUnicodeChars(actual));
	var _v5 = _v4.a;
	var unicodeValueBelow = _v5.a;
	var unicodeDiffArrowsBelow = _v5.b;
	var _v6 = _v4.b;
	var unicodeDiffArrowsAbove = _v6.a;
	var unicodeValueAbove = _v6.b;
	return A3(
		$author$project$Runner$String$Format$verticalBar,
		operation,
		(!_Utils_eq(
			A2($elm$core$String$join, '', valueBelow),
			A2($elm$core$String$join, '', unicodeValueBelow))) ? combine(
			_List_fromArray(
				[
					valueBelow,
					diffArrowsBelow,
					_Utils_ap(
					unicodeValueBelow,
					_List_fromArray(
						[' (same string but with unicode characters escaped)'])),
					unicodeDiffArrowsBelow
				])) : combine(
			_List_fromArray(
				[valueBelow, diffArrowsBelow])),
		(!_Utils_eq(
			A2($elm$core$String$join, '', valueAbove),
			A2($elm$core$String$join, '', unicodeValueAbove))) ? combine(
			_List_fromArray(
				[
					unicodeDiffArrowsAbove,
					_Utils_ap(
					unicodeValueAbove,
					_List_fromArray(
						[' (same string but with unicode characters escaped)'])),
					diffArrowsAbove,
					valueAbove
				])) : combine(
			_List_fromArray(
				[diffArrowsAbove, valueAbove])));
};
var $author$project$Runner$String$Format$listDiffToString = F4(
	function (index, description, _v0, originals) {
		listDiffToString:
		while (true) {
			var expected = _v0.expected;
			var actual = _v0.actual;
			var _v1 = _Utils_Tuple2(expected, actual);
			if (!_v1.a.b) {
				if (!_v1.b.b) {
					return A2(
						$elm$core$String$join,
						'',
						_List_fromArray(
							[
								'Two lists were unequal previously, yet ended up equal later.',
								'This should never happen!',
								'Please report this bug to https://github.com/elm-explorations/test/issues - and include these lists: ',
								'\n',
								$elm$core$Debug$toString(originals.originalExpected),
								'\n',
								$elm$core$Debug$toString(originals.originalActual)
							]));
				} else {
					var _v3 = _v1.b;
					return A3(
						$author$project$Runner$String$Format$verticalBar,
						description + ' was longer than',
						$elm$core$Debug$toString(originals.originalExpected),
						$elm$core$Debug$toString(originals.originalActual));
				}
			} else {
				if (!_v1.b.b) {
					var _v2 = _v1.a;
					return A3(
						$author$project$Runner$String$Format$verticalBar,
						description + ' was shorter than',
						$elm$core$Debug$toString(originals.originalExpected),
						$elm$core$Debug$toString(originals.originalActual));
				} else {
					var _v4 = _v1.a;
					var firstExpected = _v4.a;
					var restExpected = _v4.b;
					var _v5 = _v1.b;
					var firstActual = _v5.a;
					var restActual = _v5.b;
					if (_Utils_eq(firstExpected, firstActual)) {
						var $temp$index = index + 1,
							$temp$description = description,
							$temp$_v0 = {actual: restActual, expected: restExpected},
							$temp$originals = originals;
						index = $temp$index;
						description = $temp$description;
						_v0 = $temp$_v0;
						originals = $temp$originals;
						continue listDiffToString;
					} else {
						return A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									A3(
									$author$project$Runner$String$Format$verticalBar,
									description,
									$elm$core$Debug$toString(originals.originalExpected),
									$elm$core$Debug$toString(originals.originalActual)),
									'\n\nThe first diff is at index ',
									$elm$core$Debug$toString(index),
									': it was `',
									firstActual,
									'`, but `',
									firstExpected,
									'` was expected.'
								]));
					}
				}
			}
		}
	});
var $author$project$Runner$String$Format$format = F2(
	function (description, reason) {
		switch (reason.$) {
			case 'Custom':
				return description;
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				return $author$project$Runner$String$Format$equalityToString(
					{actual: actual, expected: expected, operation: description});
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return A3($author$project$Runner$String$Format$verticalBar, description, first, second);
			case 'TODO':
				return description;
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v1 = reason.a;
					return (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
				} else {
					return description;
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A4(
					$author$project$Runner$String$Format$listDiffToString,
					0,
					description,
					{actual: actual, expected: expected},
					{originalActual: actual, originalExpected: expected});
			default:
				var expected = reason.a.expected;
				var actual = reason.a.actual;
				var extra = reason.a.extra;
				var missing = reason.a.missing;
				var missingStr = $elm$core$List$isEmpty(missing) ? '' : ('\nThese keys are missing: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', missing)));
				var extraStr = $elm$core$List$isEmpty(extra) ? '' : ('\nThese keys are extra: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', extra)));
				return A2(
					$elm$core$String$join,
					'',
					_List_fromArray(
						[
							A3($author$project$Runner$String$Format$verticalBar, description, expected, actual),
							'\n',
							extraStr,
							missingStr
						]));
		}
	});
var $elm_explorations$test$Test$Runner$getDistributionReport = function (expectation) {
	if (expectation.$ === 'Pass') {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	} else {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	}
};
var $elm_explorations$test$Test$Runner$getFailureReason = function (expectation) {
	if (expectation.$ === 'Pass') {
		return $elm$core$Maybe$Nothing;
	} else {
		var record = expectation.a;
		return $elm$core$Maybe$Just(
			{description: record.description, given: record.given, reason: record.reason});
	}
};
var $author$project$Runner$String$indentLines = function (str) {
	return A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			$elm$core$Basics$append('    '),
			A2($elm$core$String$split, '\n', str)));
};
var $elm_explorations$test$Test$Runner$formatLabels = F3(
	function (formatDescription, formatTest, labels) {
		var _v0 = A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			labels);
		if (!_v0.b) {
			return _List_Nil;
		} else {
			var test = _v0.a;
			var descriptions = _v0.b;
			return $elm$core$List$reverse(
				A2(
					$elm$core$List$cons,
					formatTest(test),
					A2($elm$core$List$map, formatDescription, descriptions)));
		}
	});
var $author$project$Runner$String$outputLabels = function (labels) {
	return A2(
		$elm$core$String$join,
		'\n',
		A3(
			$elm_explorations$test$Test$Runner$formatLabels,
			$elm$core$Basics$append('↓ '),
			$elm$core$Basics$append('✗ '),
			labels));
};
var $elm_explorations$test$AsciiTable$AlignLeft = {$: 'AlignLeft'};
var $elm_explorations$test$AsciiTable$AlignRight = {$: 'AlignRight'};
var $elm_explorations$test$Test$Runner$Distribution$bars = 30;
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm_explorations$test$Test$Runner$Distribution$barView = function (_v0) {
	var count = _v0.count;
	var runsElapsed = _v0.runsElapsed;
	var percentage = count / runsElapsed;
	var barsForPercentage = percentage * $elm_explorations$test$Test$Runner$Distribution$bars;
	var fullBars = $elm$core$Basics$round(barsForPercentage);
	return A3(
		$elm$core$String$padRight,
		$elm_explorations$test$Test$Runner$Distribution$bars,
		_Utils_chr('░'),
		A2($elm$core$String$repeat, fullBars, '█'));
};
var $elm$core$List$map3 = _List_map3;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_explorations$test$MicroListExtra$rowsLength = function (listOfLists) {
	if (!listOfLists.b) {
		return 0;
	} else {
		var x = listOfLists.a;
		return $elm$core$List$length(x);
	}
};
var $elm_explorations$test$MicroListExtra$transpose = function (listOfLists) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$List$map2($elm$core$List$cons),
		A2(
			$elm$core$List$repeat,
			$elm_explorations$test$MicroListExtra$rowsLength(listOfLists),
			_List_Nil),
		listOfLists);
};
var $elm_explorations$test$AsciiTable$view = F2(
	function (columns, items) {
		var padFn = F3(
			function (length, align, string) {
				if (align.$ === 'AlignLeft') {
					return A3(
						$elm$core$String$padRight,
						length,
						_Utils_chr(' '),
						string);
				} else {
					return A3(
						$elm$core$String$padLeft,
						length,
						_Utils_chr(' '),
						string);
				}
			});
		var columnData = A2(
			$elm$core$List$map,
			function (col) {
				return A2($elm$core$List$map, col.toString, items);
			},
			columns);
		var columnLengths = A2(
			$elm$core$List$map,
			function (colRows) {
				return A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$maximum(
						A2($elm$core$List$map, $elm$core$String$length, colRows)));
			},
			columnData);
		var paddedColumnData = A4(
			$elm$core$List$map3,
			F3(
				function (col, colLength, colStrings) {
					return A2(
						$elm$core$List$map,
						A2(padFn, colLength, col.align),
						colStrings);
				}),
			columns,
			columnLengths,
			columnData);
		return A3(
			$elm$core$List$map2,
			F2(
				function (item, rowCells) {
					return {
						item: item,
						renderedRow: A2($elm$core$String$join, '  ', rowCells)
					};
				}),
			items,
			$elm_explorations$test$MicroListExtra$transpose(paddedColumnData));
	});
var $elm_explorations$test$Test$Runner$Distribution$viewLabels = function (labels) {
	return $elm$core$List$isEmpty(labels) ? '<uncategorized>' : A2($elm$core$String$join, ', ', labels);
};
var $elm_explorations$test$Test$Runner$Distribution$formatAsciiTable = F2(
	function (runsElapsed, items) {
		return A2(
			$elm_explorations$test$AsciiTable$view,
			_List_fromArray(
				[
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v0) {
						var labels = _v0.a;
						return '  ' + ($elm_explorations$test$Test$Runner$Distribution$viewLabels(labels) + ':');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v1) {
						var percentage = _v1.c;
						return $elm$core$String$fromFloat(percentage) + '%';
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v2) {
						var count = _v2.b;
						return '(' + ($elm$core$String$fromInt(count) + 'x)');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v3) {
						var count = _v3.b;
						return $elm_explorations$test$Test$Runner$Distribution$barView(
							{count: count, runsElapsed: runsElapsed});
					}
				}
				]),
			items);
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$diff, dict1, dict2));
	});
var $elm_explorations$test$Test$Runner$Distribution$isStrictSubset = F2(
	function (all, combination) {
		var combinationSet = $elm$core$Set$fromList(combination);
		var containsCombinationFully = function (set) {
			return (!$elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, set, combinationSet))) && $elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, combinationSet, set));
		};
		var allSets = A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $elm$core$Set$fromList),
			all);
		return A2($elm$core$List$any, containsCombinationFully, allSets);
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm_explorations$test$MicroListExtra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(index);
				} else {
					var $temp$index = index + 1,
						$temp$predicate = predicate,
						$temp$list = xs;
					index = $temp$index;
					predicate = $temp$predicate;
					list = $temp$list;
					continue findIndexHelp;
				}
			}
		}
	});
var $elm_explorations$test$MicroListExtra$findIndex = $elm_explorations$test$MicroListExtra$findIndexHelp(0);
var $elm_explorations$test$MicroListExtra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $elm_explorations$test$MicroListExtra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			$elm$core$Maybe$map,
			function (i) {
				return A2($elm_explorations$test$MicroListExtra$splitAt, i, list);
			},
			A2($elm_explorations$test$MicroListExtra$findIndex, predicate, list));
	});
var $elm_explorations$test$Test$Runner$Distribution$formatTable = function (_v0) {
	var runsElapsed = _v0.runsElapsed;
	var distributionCount = _v0.distributionCount;
	var runsElapsed_ = runsElapsed;
	var distributionList = $elm$core$Dict$toList(distributionCount);
	var distribution = A2(
		$elm$core$List$map,
		function (_v8) {
			var labels = _v8.a;
			var count = _v8.b;
			var percentage = $elm$core$Basics$round((count / runsElapsed_) * 1000) / 10;
			return _Utils_Tuple3(labels, count, percentage);
		},
		A2(
			$elm$core$List$filter,
			function (_v7) {
				var labels = _v7.a;
				var count = _v7.b;
				return !(($elm$core$List$length(labels) === 1) && ((!count) && A2($elm_explorations$test$Test$Runner$Distribution$isStrictSubset, distributionList, labels)));
			},
			distributionList));
	var _v1 = A2(
		$elm$core$List$partition,
		function (_v3) {
			var labels = _v3.a;
			return $elm$core$List$length(labels) <= 1;
		},
		A2(
			$elm$core$List$sortBy,
			function (_v2) {
				var count = _v2.b;
				return -count;
			},
			distribution));
	var baseRows = _v1.a;
	var combinationsRows = _v1.b;
	var reorderedTable = _Utils_ap(baseRows, combinationsRows);
	var rawTable = A2($elm_explorations$test$Test$Runner$Distribution$formatAsciiTable, runsElapsed_, reorderedTable);
	var _v4 = A2(
		$elm$core$Maybe$withDefault,
		_Utils_Tuple2(rawTable, _List_Nil),
		A2(
			$elm_explorations$test$MicroListExtra$splitWhen,
			function (_v5) {
				var item = _v5.item;
				var _v6 = item;
				var labels = _v6.a;
				return $elm$core$List$length(labels) > 1;
			},
			rawTable));
	var base = _v4.a;
	var combinations = _v4.b;
	var baseString = A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			function ($) {
				return $.renderedRow;
			},
			base));
	var combinationsString_ = $elm$core$List$isEmpty(combinations) ? '' : A3(
		$elm$core$String$replace,
		'{COMBINATIONS}',
		A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.renderedRow;
				},
				combinations)),
		'\n\nCombinations (included in the above base counts):\n{COMBINATIONS}');
	var table = _Utils_ap(baseString, combinationsString_);
	return A3($elm$core$String$replace, '{CATEGORIES}', table, 'Distribution report:\n====================\n{CATEGORIES}');
};
var $elm_explorations$test$Test$Distribution$distributionReportTable = function (r) {
	return $elm_explorations$test$Test$Runner$Distribution$formatTable(r);
};
var $author$project$Runner$String$Distribution$report = F2(
	function (testBreadcrumbs, distributionReport) {
		switch (distributionReport.$) {
			case 'NoDistribution':
				return $elm$core$Maybe$Nothing;
			case 'DistributionToReport':
				var r = distributionReport.a;
				var breadcrumbsPath = A2(
					$elm$core$String$join,
					' > ',
					$elm$core$List$reverse(testBreadcrumbs));
				return $elm$core$Maybe$Just(
					breadcrumbsPath + ('\n' + $elm_explorations$test$Test$Distribution$distributionReportTable(r)));
			case 'DistributionCheckSucceeded':
				return $elm$core$Maybe$Nothing;
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Runner$String$fromExpectation = F3(
	function (labels, expectation, summary) {
		var distributionReport = A2(
			$author$project$Runner$String$Distribution$report,
			labels,
			$elm_explorations$test$Test$Runner$getDistributionReport(expectation));
		var summaryWithDistribution = function () {
			if (distributionReport.$ === 'Nothing') {
				return summary;
			} else {
				var distribution = distributionReport.a;
				return _Utils_update(
					summary,
					{output: summary.output + ('\n\n' + (distribution + '\n'))});
			}
		}();
		var _v0 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
		if (_v0.$ === 'Nothing') {
			return _Utils_update(
				summaryWithDistribution,
				{passed: summaryWithDistribution.passed + 1});
		} else {
			var given = _v0.a.given;
			var description = _v0.a.description;
			var reason = _v0.a.reason;
			var prefix = function () {
				if (given.$ === 'Nothing') {
					return '';
				} else {
					var g = given.a;
					return 'Given ' + (g + '\n\n');
				}
			}();
			var message = A2($author$project$Runner$String$Format$format, description, reason);
			var newOutput = '\n\n' + ($author$project$Runner$String$outputLabels(labels) + ('\n' + (_Utils_ap(
				prefix,
				$author$project$Runner$String$indentLines(message)) + '\n')));
			return _Utils_update(
				summaryWithDistribution,
				{
					failed: summaryWithDistribution.failed + 1,
					output: _Utils_ap(summaryWithDistribution.output, newOutput),
					passed: summaryWithDistribution.passed
				});
		}
	});
var $author$project$Runner$String$toOutputHelp = F2(
	function (runner, summary) {
		return A3(
			$elm$core$List$foldl,
			$author$project$Runner$String$fromExpectation(runner.labels),
			summary,
			runner.run(_Utils_Tuple0));
	});
var $author$project$Runner$String$toOutput = F2(
	function (summary, seededRunners) {
		var render = $elm$core$List$foldl($author$project$Runner$String$toOutputHelp);
		switch (seededRunners.$) {
			case 'Plain':
				var runners = seededRunners.a;
				return A2(
					render,
					_Utils_update(
						summary,
						{autoFail: $elm$core$Maybe$Nothing}),
					runners);
			case 'Only':
				var runners = seededRunners.a;
				return A2(
					render,
					_Utils_update(
						summary,
						{
							autoFail: $elm$core$Maybe$Just('Test.only was used')
						}),
					runners);
			case 'Skipping':
				var runners = seededRunners.a;
				return A2(
					render,
					_Utils_update(
						summary,
						{
							autoFail: $elm$core$Maybe$Just('Test.skip was used')
						}),
					runners);
			default:
				var message = seededRunners.a;
				return {autoFail: $elm$core$Maybe$Nothing, failed: 0, output: message, passed: 0};
		}
	});
var $author$project$Runner$String$runWithOptions = F3(
	function (runs, seed, test) {
		var seededRunners = A3($elm_explorations$test$Test$Runner$fromTest, runs, seed, test);
		return A2(
			$author$project$Runner$String$toOutput,
			{
				autoFail: $elm$core$Maybe$Just('no tests were run'),
				failed: 0,
				output: '',
				passed: 0
			},
			seededRunners);
	});
var $author$project$EffectivenessMain$runCount = F2(
	function (seed, fuzz) {
		return A3(
			$author$project$Runner$String$runWithOptions,
			fuzz,
			$elm$random$Random$initialSeed(seed),
			$author$project$EffectivenessTests$all).failed;
	});
var $author$project$EffectivenessMain$binarySearch = F4(
	function (seed, target, low, high) {
		binarySearch:
		while (true) {
			if (_Utils_cmp(low, high) > -1) {
				return low;
			} else {
				var mid = ((low + high) / 2) | 0;
				var failed = A2($author$project$EffectivenessMain$runCount, seed, mid);
				if (_Utils_cmp(failed, target) > -1) {
					var $temp$seed = seed,
						$temp$target = target,
						$temp$low = low,
						$temp$high = mid;
					seed = $temp$seed;
					target = $temp$target;
					low = $temp$low;
					high = $temp$high;
					continue binarySearch;
				} else {
					var $temp$seed = seed,
						$temp$target = target,
						$temp$low = mid + 1,
						$temp$high = high;
					seed = $temp$seed;
					target = $temp$target;
					low = $temp$low;
					high = $temp$high;
					continue binarySearch;
				}
			}
		}
	});
var $author$project$EffectivenessMain$findBoundsHelp = F4(
	function (seed, target, fuzz, cap) {
		findBoundsHelp:
		while (true) {
			var failed = A2($author$project$EffectivenessMain$runCount, seed, fuzz);
			if ((_Utils_cmp(failed, target) > -1) || (_Utils_cmp(fuzz, cap) > -1)) {
				return _Utils_Tuple2(
					A2($elm$core$Basics$max, 1, (fuzz / 2) | 0),
					fuzz);
			} else {
				var $temp$seed = seed,
					$temp$target = target,
					$temp$fuzz = fuzz * 2,
					$temp$cap = cap;
				seed = $temp$seed;
				target = $temp$target;
				fuzz = $temp$fuzz;
				cap = $temp$cap;
				continue findBoundsHelp;
			}
		}
	});
var $author$project$EffectivenessMain$findBounds = F3(
	function (seed, target, cap) {
		return A4($author$project$EffectivenessMain$findBoundsHelp, seed, target, 1, cap);
	});
var $author$project$EffectivenessMain$minimalFuzzForSeed = F3(
	function (target, cap, seed) {
		var _v0 = A3($author$project$EffectivenessMain$findBounds, seed, target, cap);
		var low = _v0.a;
		var high = _v0.b;
		var failed = A2($author$project$EffectivenessMain$runCount, seed, high);
		return (_Utils_cmp(failed, target) > -1) ? $elm$core$Maybe$Just(
			A4($author$project$EffectivenessMain$binarySearch, seed, target, low, high)) : $elm$core$Maybe$Nothing;
	});
var $author$project$EffectivenessMain$measure = F2(
	function (cfg, seeds) {
		var digits = $elm$core$String$length(
			$elm$core$String$fromInt(cfg.totalSeeds));
		var step = F2(
			function (seed, acc) {
				var _v0 = A3($author$project$EffectivenessMain$minimalFuzzForSeed, cfg.targetFailures, cfg.maxFuzz, seed);
				if (_v0.$ === 'Just') {
					var n = _v0.a;
					var _v1 = A2(
						$elm$core$Debug$log,
						A3(
							$elm$core$String$replace,
							'{seed}',
							A3(
								$elm$core$String$padLeft,
								digits,
								_Utils_chr(' '),
								$elm$core$String$fromInt(seed)),
							'  Seed {seed}: minimal fuzz'),
						n);
					return A2($elm$core$List$cons, n, acc);
				} else {
					var _v2 = A2(
						$elm$core$Debug$log,
						A3(
							$elm$core$String$replace,
							'{seed}',
							$elm$core$String$fromInt(seed),
							'  Seed {seed}: did not reach target'),
						_Utils_Tuple0);
					return acc;
				}
			});
		return A3($elm$core$List$foldl, step, _List_Nil, seeds);
	});
var $author$project$EffectivenessMain$medianOfSorted = function (sorted) {
	if (!sorted.b) {
		return 0;
	} else {
		if (!sorted.b.b) {
			var x = sorted.a;
			return x;
		} else {
			var len = $elm$core$List$length(sorted);
			var half = (len / 2) | 0;
			var dropped = A2($elm$core$List$drop, half - 1, sorted);
			if (A2($elm$core$Basics$modBy, 2, len) === 1) {
				return A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$head(
						A2($elm$core$List$drop, 1, dropped)));
			} else {
				var b = A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$head(
						A2($elm$core$List$drop, 1, dropped)));
				var a = A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$head(dropped));
				return ((a + b) / 2) | 0;
			}
		}
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$EffectivenessMain$report = F2(
	function (total, values) {
		var reached = $elm$core$List$length(values);
		if (!reached) {
			return $elm$core$Maybe$Nothing;
		} else {
			var sum = $elm$core$List$sum(values);
			var sorted = $elm$core$List$sort(values);
			var minVal = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$List$minimum(values));
			var median = $author$project$EffectivenessMain$medianOfSorted(sorted);
			var maxVal = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$List$maximum(values));
			var avg = sum / reached;
			return $elm$core$Maybe$Just(
				{avg: avg, max: maxVal, median: median, min: minVal, reached: reached});
		}
	});
var $author$project$EffectivenessMain$roundTo = F2(
	function (decimals, x) {
		var p = A2($elm$core$Basics$pow, 10, decimals);
		return $elm$core$Basics$round(x * p) / p;
	});
var $author$project$EffectivenessMain$seedList = F3(
	function (totalSeeds, multiplier, addend) {
		var k = ((totalSeeds - addend) / multiplier) | 0;
		return ((_Utils_cmp(addend, totalSeeds) > 0) || (k < 0)) ? _List_Nil : A2(
			$elm$core$List$map,
			function (i) {
				return addend + (i * multiplier);
			},
			A2($elm$core$List$range, 0, k));
	});
var $author$project$EffectivenessMain$run = function (cfg) {
	var seeds = A3($author$project$EffectivenessMain$seedList, cfg.totalSeeds, cfg.multiplier, cfg.addend);
	var _v0 = A2(
		$elm$core$Debug$log,
		A3(
			$elm$core$String$replace,
			'{count}',
			$elm$core$String$fromInt(
				$elm$core$List$length(seeds)),
			A3(
				$elm$core$String$replace,
				'{addend}',
				$elm$core$String$fromInt(cfg.addend),
				A3(
					$elm$core$String$replace,
					'{target}',
					$elm$core$String$fromInt(cfg.targetFailures),
					'Target {target} failures, seeds (addend {addend}): {count} seeds'))),
		_Utils_Tuple0);
	var _v1 = A2(
		$author$project$EffectivenessMain$report,
		$elm$core$List$length(seeds),
		A2($author$project$EffectivenessMain$measure, cfg, seeds));
	if (_v1.$ === 'Nothing') {
		return A2(
			$elm$core$Debug$log,
			A3(
				$elm$core$String$replace,
				'{maxFuzz}',
				$elm$core$String$fromInt(cfg.maxFuzz),
				A3(
					$elm$core$String$replace,
					'{target}',
					$elm$core$String$fromInt(cfg.targetFailures),
					'No seed in slice reached {target} failures (maxFuzz = {maxFuzz}).')),
			_Utils_Tuple0);
	} else {
		var reached = _v1.a.reached;
		var min = _v1.a.min;
		var max = _v1.a.max;
		var avg = _v1.a.avg;
		var median = _v1.a.median;
		return A2(
			$elm$core$Debug$log,
			A3(
				$elm$core$String$replace,
				'{median}',
				$elm$core$String$fromInt(median),
				A3(
					$elm$core$String$replace,
					'{avg}',
					$elm$core$String$fromFloat(
						A2($author$project$EffectivenessMain$roundTo, 1, avg)),
					A3(
						$elm$core$String$replace,
						'{max}',
						$elm$core$String$fromInt(max),
						A3(
							$elm$core$String$replace,
							'{min}',
							$elm$core$String$fromInt(min),
							A3(
								$elm$core$String$replace,
								'{addend}',
								$elm$core$String$fromInt(cfg.addend),
								A3(
									$elm$core$String$replace,
									'{total}',
									$elm$core$String$fromInt(
										$elm$core$List$length(seeds)),
									A3(
										$elm$core$String$replace,
										'{reached}',
										$elm$core$String$fromInt(reached),
										'\n{reached}/{total} reached target (addend {addend})\nmin = {min}\nmax = {max}\navg = {avg}\np50 = {median}\n\nFinished!'))))))),
			_Utils_Tuple0);
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$EffectivenessMain$main = $elm$core$Platform$worker(
	{
		init: function (flagsValue) {
			var cfg = function () {
				var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$EffectivenessMain$decodeFlagsWithDefaults, flagsValue);
				if (_v1.$ === 'Ok') {
					var c = _v1.a;
					return c;
				} else {
					return $author$project$EffectivenessMain$defaultConfig;
				}
			}();
			var _v0 = $author$project$EffectivenessMain$run(cfg);
			return _Utils_Tuple2(_Utils_Tuple0, $elm$core$Platform$Cmd$none);
		},
		subscriptions: function (_v2) {
			return $elm$core$Platform$Sub$none;
		},
		update: F2(
			function (_v3, s) {
				return _Utils_Tuple2(s, $elm$core$Platform$Cmd$none);
			})
	});
_Platform_export({'EffectivenessMain':{'init':$author$project$EffectivenessMain$main($elm$json$Json$Decode$value)(0)}});}(this));