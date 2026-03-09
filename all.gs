//   Copyright (C) 2026 lattiahirvio
//
//   This file is part of Clojette.
//
//   Clojette is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   any later version.
//
//   Clojette is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with Clojette. If not, see <https://www.gnu.org/licenses/>.

__runtimeTag__ = function
	
end function

lispError = function(msg)
	  print("ERROR: "+ msg)
    return {"classID": "error", "__tag__": @__runtimeTag__, "message": msg}
end function

// -----------------------------------------------------------------------------------
// ENV.src
// -----------------------------------------------------------------------------------
// Environment setup, very cool.
makeEnv = function(outerEnv)
    e = {}
    e.locals = {}
    e.get = function(name)
        if self.locals.hasIndex(name) then return @self.locals[name]
        if outerEnv != null then return outerEnv.get(name)
        return lispError("Undefined in the env: " + name)
    end function
    e.set = function(name, value)
        self.locals[name] = @value
    end function
	e.setExisting = function(name, value)
    	if self.locals.hasIndex(name) then
        	self.locals[name] = value
        	return @value
    	end if
    	if outerEnv != null then return outerEnv.setExisting(name, @value)
    	return lispError("Cannot set! undefined variable: " + name)
	end function
    return e
end function

bindArgs = function(argNames, params, baseEnv)
    newEnv = makeEnv(baseEnv)
    
    // No args expected
    if argNames.len == 0 then
        if params.len > 0 then
            return lispError("Wrong number of args: expected 0, got " + params.len)
        end if
        return newEnv
    end if
    
    // Find & position if present
    restIdx = null
    for i in range(0, argNames.len-1)
        if argNames[i] == "&" then
            restIdx = i
            break
        end if
    end for
    
    if restIdx != null then
        // Variadic: minimum arity is everything before the &
        if params.len < restIdx then
            return lispError("Wrong number of args: expected at least " + restIdx + ", got " + params.len)
        end if
        for i in range(0, restIdx-1)
            newEnv.set(argNames[i], params[i])
        end for
        restName = argNames[restIdx+1]
        // Gracefully bind empty list if no rest args provided
        if restIdx >= params.len then
            newEnv.set(restName, [])
        else
            newEnv.set(restName, params[restIdx:])
        end if
    else
        // Exact arity required
        if params.len != argNames.len then
            return lispError("Wrong number of args: expected " + argNames.len + ", got " + params.len)
        end if
        for i in range(0, argNames.len-1)
            newEnv.set(argNames[i], params[i])
        end for
    end if
    
    return newEnv
end function

Env = {}
globalEnv = makeEnv(null)
// In the MiniScript bootstrap, before the REPL
globalEnv.locals["__recur_sentinel__"] = {"classID": "recur", "args": null}
globalEnv.locals["__gensym_counter__"] = 0
globalEnv.locals["macros"] = {}
globalEnv.natives = {}


// -----------------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------------

// sentinels for the env, lets us use special forms from macros.
// Yes this is a non-ideal, but what can you do? 
// TODO: fix
globalEnv.locals["do"] = "do"
globalEnv.locals["if"] = "if"
globalEnv.locals["def"] = "def"
globalEnv.locals["fn"] = "fn"
globalEnv.locals["let"] = "let"
globalEnv.locals["quote"] = "quote"
globalEnv.locals["set!"] = "set!"

//
// Clojette Builtins - MiniScript host layer
//
globalEnv.locals["gensym"] = function(args)
    prefix = "G__"
    if args.len > 0 then prefix = args[0]
    __gensym_counter__ = globalEnv.locals["__gensym_counter__"] + 1
    globalEnv.locals["__gensym_counter__"] = __gensym_counter__
    return prefix + __gensym_counter__
end function

// Arithmetic
globalEnv.locals["+"] = function(args)
    sum = 0
    if args.len == 0 then return 0
    for i in range(0, args.len-1)
        sum = sum + args[i]
    end for
    return sum
end function

globalEnv.locals["-"] = function(args)
    if args.len == 0 then return lispError("- requires at least 1 argument")
    if args.len == 1 then return -args[0]
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            result = result - args[i]
        end for
    end if
    return result
end function

globalEnv.locals["*"] = function(args)
    prod = 1
    if args.len == 0 then return 1
    for i in range(0, args.len-1)
        prod = prod * args[i]
    end for
    return prod
end function

globalEnv.locals["/"] = function(args)
    if args.len == 0 then return lispError("/ requires at least 1 argument")
    if args.len == 1 then return 1.0 / args[0]
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            if args[i] == 0 then return lispError("Division by zero")
            result = result / args[i]
        end for
    end if
    return result
end function

globalEnv.locals["%"] = function(args)
    if args.len != 2 then return lispError("% requires exactly 2 arguments")
    if args[1] == 0 then return lispError("Modulo by zero")
    return args[0] % args[1]
end function

globalEnv.locals["mod"] = function(args)
    if args.len != 2 then return lispError("mod requires exactly 2 arguments")
    if args[1] == 0 then return lispError("Modulo by zero")
    return args[0] % args[1]
end function

globalEnv.locals["**"] = function(args)
    if args.len != 2 then return lispError("** requires exactly 2 arguments")
    return args[0] ^ args[1]
end function

globalEnv.locals["quot"] = function(args)
    if args.len != 2 then return lispError("quot requires exactly 2 arguments")
    if args[1] == 0 then return lispError("Division by zero")
    return floor(args[0] / args[1])
end function

// Comparison
globalEnv.locals["="] = function(args)
    if args.len < 2 then return lispError("= requires at least 2 arguments")
    for i in range(1, args.len-1)
        if args[i] != args[0] then return false
    end for
    return true
end function

globalEnv.locals["not="] = function(args)
    if args.len != 2 then return lispError("not= requires exactly 2 arguments")
    return args[0] != args[1]
end function

globalEnv.locals["<"] = function(args)
    if args.len < 2 then return lispError("< requires at least 2 arguments")
    for i in range(1, args.len-1)
        if args[i-1] >= args[i] then return false
    end for
    return true
end function

globalEnv.locals[">"] = function(args)
    if args.len < 2 then return lispError("> requires at least 2 arguments")
    for i in range(1, args.len-1)
        if args[i-1] <= args[i] then return false
    end for
    return true
end function

globalEnv.locals["<="] = function(args)
    if args.len < 2 then return lispError("<= requires at least 2 arguments")
    for i in range(1, args.len-1)
        if args[i-1] > args[i] then return false
    end for
    return true
end function

globalEnv.locals[">="] = function(args)
    if args.len < 2 then return lispError(">= requires at least 2 arguments")
    for i in range(1, args.len-1)
        if args[i-1] < args[i] then return false
    end for
    return true
end function

globalEnv.locals["not"] = function(args)
    if args.len != 1 then return lispError("not requires exactly 1 argument")
    return not args[0]
end function

// List operations
globalEnv.locals["list"] = function(args)
	  if args == null then return lispError("Args for list is null!")
    return [] + args
end function

globalEnv.locals["car"] = function(args)
    if args.len != 1 then return lispError("car requires exactly 1 argument")
    lst = args[0]
    if lst == null or lst.len == 0 then return lispError("car called on empty list")
    return lst[0]
end function

globalEnv.locals["cdr"] = function(args)
    if args.len != 1 then return lispError("cdr requires exactly 1 argument")
    lst = args[0]
    if lst == null or lst.len == 0 then return []  // was: lispError
    if lst.len == 1 then return []
    return lst[1:]
end function

globalEnv.locals["cons"] = function(args)
    if args.len != 2 then return lispError("cons requires exactly 2 arguments")
    if args[1] == null then return [args[0]]
    return [args[0]] + args[1]
end function

globalEnv.locals["first"] = function(args)
    if args.len != 1 then return lispError("first requires exactly 1 argument")
    lst = args[0]
    if lst == null or lst.len == 0 then return null
    return lst[0]
end function

globalEnv.locals["second"] = function(args)
    if args.len != 1 then return lispError("second requires exactly 1 argument")
    lst = args[0]
    if lst == null or lst.len < 2 then return null
    return lst[1]
end function

globalEnv.locals["rest"] = function(args)
    if args.len != 1 then return lispError("rest requires exactly 1 argument")
    lst = args[0]
    if lst == null or lst.len <= 1 then return []
    return lst[1:]
end function

globalEnv.locals["conj"] = function(args)
    if args.len < 2 then return lispError("conj requires at least 2 arguments")
    result = args[0]
    if result == null then result = []
    if args.len > 1 then
        for i in range(1, args.len-1)
            result = result + [args[i]]
        end for
    end if
    return result
end function

globalEnv.locals["concat"] = function(args)
    result = []
    if args.len == 0 then return result
    for i in range(0, args.len-1)
        if args[i] != null then result = result + args[i]
    end for
    return result
end function

globalEnv.locals["empty?"] = function(args)
	if isError(args) then return args
    if args.len != 1 then return lispError("empty? requires exactly 1 argument")
    lst = args[0]
    if lst == null then return true
    return lst.len == 0
end function

globalEnv.locals["count"] = function(args)
    if args.len != 1 then return lispError("count requires exactly 1 argument")
    if args[0] == null then return 0
    return args[0].len
end function

globalEnv.locals["list?"] = function(args)
    if args.len != 1 then return lispError("list? requires exactly 1 argument")
    return args[0] isa list
end function

globalEnv.locals["nth"] = function(args)
    if args.len != 2 then return lispError("nth requires exactly 2 arguments")
    lst = args[0]
    n = args[1]
    if lst == null or n >= lst.len then return lispError("nth index out of bounds")
    return lst[n]
end function

globalEnv.locals["get"] = function(args)
    if args.len < 2 then return lispError("get requires at least 2 arguments")
    coll = args[0]
    key = args[1]
    if coll == null then return null
    if not coll.hasIndex(key) then
        if args.len == 3 then return args[2]
        return null
    end if
    return @coll[key]
end function

// Map/dict operations
globalEnv.locals["hash-map"] = function(args)
    result = {}
    if args.len == 0 then return result
    if args.len % 2 != 0 then return lispError("hash-map requires even number of arguments")
    for i in range(0, args.len-1, 2)
        result[args[i]] = @args[i+1]
    end for
    return result
end function

globalEnv.locals["assoc"] = function(args)
    if args.len < 3 then return lispError("assoc requires at least 3 arguments")
    result = {}
    if args[0] != null then
        for kv in args[0]
            result[kv.key] = @kv.value
        end for
    end if
    if args.len > 1 then
        for i in range(1, args.len-1, 2)
            result[args[i]] = @args[i+1]
        end for
    end if
    return result
end function

globalEnv.locals["dissoc"] = function(args)
    if args.len < 2 then return lispError("dissoc requires at least 2 arguments")
    result = {}
    for kv in args[0]
        result[kv.key] = @kv.value
    end for
    if args.len > 1 then
        for i in range(1, args.len-1)
            result.remove(args[i])
        end for
    end if
    return result
end function

globalEnv.locals["keys"] = function(args)
    if args.len != 1 then return lispError("keys requires exactly 1 argument")
    if args[0] == null then return []
    result = []
    for kv in args[0]
        result.push(kv.key)
    end for
    return result
end function

globalEnv.locals["vals"] = function(args)
    if args.len != 1 then return lispError("vals requires exactly 1 argument")
    if args[0] == null then return []
    result = []
    for kv in args[0]
        result.push(@kv.value)
    end for
    return result
end function

globalEnv.locals["map?"] = function(args)
    if args.len != 1 then return lispError("map? requires exactly 1 argument")
    return args[0] isa map
end function

globalEnv.locals["contains?"] = function(args)
    if args.len != 2 then return lispError("contains? requires exactly 2 arguments")
    if args[0] == null then return false
    return args[0].hasIndex(args[1])
end function

// Type checks
globalEnv.locals["number?"] = function(args)
    if args.len != 1 then return lispError("number? requires exactly 1 argument")
    return args[0] isa number
end function

globalEnv.locals["string?"] = function(args)
    if args.len != 1 then return lispError("string? requires exactly 1 argument")
    return args[0] isa string
end function

globalEnv.locals["null?"] = function(args)
    if args.len != 1 then return lispError("null? requires exactly 1 argument")
    return args[0] == null
end function

globalEnv.locals["fn?"] = function(args)
    if args.len != 1 then return lispError("fn? requires exactly 1 argument")
    if args[0] isa funcRef then return true
    return args[0] isa map and args[0].hasIndex("classID") and args[0]["classID"] == "fn"
end function

globalEnv.locals["true?"] = function(args)
    if args.len != 1 then return lispError("true? requires exactly 1 argument")
    return args[0] == true
end function

globalEnv.locals["false?"] = function(args)
    if args.len != 1 then return lispError("false? requires exactly 1 argument")
    return args[0] == false
end function

// Math
globalEnv.locals["floor"] = function(args)
    if args.len != 1 then return lispError("floor requires exactly 1 argument")
    return floor(args[0])
end function

globalEnv.locals["ceil"] = function(args)
    if args.len != 1 then return lispError("ceil requires exactly 1 argument")
    return ceil(args[0])
end function

globalEnv.locals["round"] = function(args)
    if args.len != 1 then return lispError("round requires exactly 1 argument")
    return round(args[0])
end function

globalEnv.locals["abs"] = function(args)
    if args.len != 1 then return lispError("abs requires exactly 1 argument")
    return abs(args[0])
end function

globalEnv.locals["sqrt"] = function(args)
    if args.len != 1 then return lispError("sqrt requires exactly 1 argument")
    return sqrt(args[0])
end function

globalEnv.locals["max"] = function(args)
    if args.len < 1 then return lispError("max requires at least 1 argument")
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            if args[i] > result then result = args[i]
        end for
    end if
    return result
end function

globalEnv.locals["min"] = function(args)
    if args.len < 1 then return lispError("min requires at least 1 argument")
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            if args[i] < result then result = args[i]
        end for
    end if
    return result
end function

// String operations
globalEnv.locals["str"] = function(args)
    result = ""
    if args.len == 0 then return result
    for i in range(0, args.len-1)
        result = result + str(args[i])
    end for
    return result
end function

globalEnv.locals["split"] = function(args)
    if args.len != 2 then return lispError("split requires exactly 2 arguments")
    return args[0].split(args[1])
end function

globalEnv.locals["join"] = function(args)
    if args.len != 2 then return lispError("join requires exactly 2 arguments")
    return args[0].join(args[1])
end function

globalEnv.locals["trim"] = function(args)
    if args.len != 1 then return lispError("trim requires exactly 1 argument")
    return args[0].trim
end function

globalEnv.locals["index-of"] = function(args)
    if args.len != 2 then return lispError("index-of requires exactly 2 arguments")
    return args[0].indexOf(args[1])
end function

globalEnv.locals["subs"] = function(args)
    if args.len < 2 then return lispError("subs requires at least 2 arguments")
    if args.len == 2 then return args[0][args[1]:]
    return args[0][args[1]:args[2]]
end function

globalEnv.locals["upper-case"] = function(args)
    if args.len != 1 then return lispError("upper-case requires exactly 1 argument")
    return args[0].upper
end function

globalEnv.locals["lower-case"] = function(args)
    if args.len != 1 then return lispError("lower-case requires exactly 1 argument")
    return args[0].lower
end function

globalEnv.locals["replace"] = function(args)
    if args.len != 3 then return lispError("replace requires exactly 3 arguments")
    haystack = args[0]
    needle = args[1]
    replacement = args[2]
    if needle == "" then return lispError("replace: needle cannot be empty")
    return haystack.replace(needle, replacement)
end function

// I/O
globalEnv.locals["println"] = function(args)
    if args.len == 0 then
        print("")
        return null
    end if
    parts = []
    for i in range(0, args.len-1)
        parts.push(str(@args[i]))
    end for
    print(parts.join(" "))
    return null
end function

globalEnv.locals["user-input"] = function(args)
    if args.len > 0 then return user_input(args[0])
    return user_input("")
end function

// Apply - needed for higher order functions
globalEnv.locals["apply"] = function(args)
    if args.len != 2 then return lispError("apply requires exactly 2 arguments")
    fn = @args[0]
    argList = args[1]
    if not argList isa list then return lispError("apply requires a list as second argument")
    return callFunction(@fn, argList, "apply")
end function

globalEnv.locals["take-keys"] = function(args)
    bindings = args[0]
    if bindings isa list and bindings.len > 0 and bindings[0] == "array" then
        bindings = bindings[1:]
    end if
    result = []
    for i in range(0, bindings.len-1, 2)
        result.push(bindings[i])
    end for
    return result
end function

globalEnv.locals["take-vals"] = function(args)
    bindings = args[0]
    if bindings isa list and bindings.len > 0 and bindings[0] == "array" then
        bindings = bindings[1:]
    end if
    result = []
    for i in range(1, bindings.len-1, 2)
        result.push(bindings[i])
    end for
    return result
end function

// Constants
globalEnv.locals["true"] = true
globalEnv.locals["false"] = false
globalEnv.locals["null"] = null
globalEnv.locals["nil"] = null

// -----------------------------------------------------------------------------------
// CORE 
// -----------------------------------------------------------------------------------

// helpers
atom = function(token)
	// We dereference the token to not invoke anything by accident
    if @token isa number then return token
	if @token isa funcRef then return lispError("Tried evaluating funcRef as an atom?")
	// Return full string literal
	if token[0] == """" then return token
    num = token.val
    if str(num) == token then return num
    return token
end function

// We can check if a given result is an error; we want error handling
isError = function(val)
    if not @val isa map then return false
    // We know that the ope is a map, and potentially is an error; safe to handle without deref 
    if not @val.hasIndex("__tag__") then return false
    return @val["__tag__"] == @__runtimeTag__
end function

isRuntimeObject = function(val)
    if not @val isa map then return false
    if not val.hasIndex("__tag__") then return false
    return @val["__tag__"] == @__runtimeTag__
end function

// There was an error here, where we were trying to check if op was a funcRef
// that check was not dereferenced and we called it directly -_-
// Lesson learned, always deref your functions
callFunction = function(op, args, name, isNative=false)
    if isError(@op) then return @op
    
    // User-defined Clojette fn
    if @op isa map then
        if op.hasIndex("classID") and op["classID"] == "fn" then
            while true
                newEnv = bindArgs(op["args"], args, op["env"])
                if isError(@newEnv) then return newEnv
                result = null
                for bodyExpr in op["body"]
                    result = eval(bodyExpr, newEnv)
                    if isError(@result) then return result
                end for
                // check if recur was signalled
                if result isa map and result.hasIndex("classID") and result["classID"] == "recur" then
                    args = result["args"]  // loop with new args
                else
                    return result  // done
                end if
            end while
        end if
    end if
    
    // funcRef - either stdlib or native MiniScript
    if @op isa funcRef or typeof(@op) == "function" then
        if isNative then
            if args.len == 0 then return @op()
            if args.len == 1 then return @op(args[0])
            if args.len == 2 then return @op(args[0], args[1])
            if args.len == 3 then return @op(args[0], args[1], args[2])
            if args.len == 4 then return @op(args[0], args[1], args[2], args[3])
            if args.len == 5 then return @op(args[0], args[1], args[2], args[3], args[4])
            return lispError("Native functions support at most 5 arguments")
        else
            return op(@args)
        end if
    end if
    
    return lispError("Not a function: " + name)
end function

evalQuasiquote = function(exp, env)
    // not a list, just return it as-is (like quote)
    if not @exp isa list then return exp
    // empty list
    if @exp.len == 0 then return exp
    
    // unquote: evaluate and return
    if exp[0] == "unquote" then
		result = eval(exp[1], env)
        if isError(@result) then return result
		return result
    end if
    
    // walk the list, handling splice-unquote
    result = []
    for i in range(0, exp.len-1)
        item = exp[i]
        if item isa list and item.len > 0 and item[0] == "splice-unquote" then
            spliced = eval(item[1], env)
			if isError(@spliced) then return spliced
    		if not @spliced isa list then return lispError("splice-unquote requires a list, got: " + typeof(@spliced))
            if spliced.len > 0 then
                for j in range(0, spliced.len-1)
                    result.push(spliced[j])
                end for
            end if
        else
    		item = evalQuasiquote(@item, env)
    		if isError(@item) then return item
    		result.push(@item)
        end if
    end for
    return result
end function

// Convert a string of characters into a list of tokens
// MiniScript, being the retarded fucker it is, sees the input as a RegEx. As a result, we have to escape the input.
tokenize = function(chars)
    tokens = []
    //if tokens.len == 0 then return lispError("Unexpected EOF")
    //if isError(tokens[0]) then return tokens.pull  // propagate tokenizer errors
    i = 0
    while i < chars.len
        c = chars[i]
		if c == """" then
    		tok = c
    		i = i + 1
    		while i < chars.len and chars[i] != """"
        		if chars[i] == "\" then
            		tok = tok + chars[i]
            		i = i + 1
            		if i < chars.len then tok = tok + chars[i]
        		else
            		tok = tok + chars[i]
        		end if
        		i = i + 1
    		end while
    		if i >= chars.len then
        		tokens.push(lispError("Unterminated string literal: " + tok))
        		return tokens
    		end if
    		tok = tok + """"
    		tokens.push(tok)
        else if c == "(" or c == ")" or c == "[" or c == "]" then
            tokens.push(c)
        else if c == "~" then
            if i + 1 < chars.len and chars[i+1] == "@" then
                tokens.push("~@")
                i = i + 1
            else
                tokens.push("~")
            end if
        else if c == "'" or c == "`" then
            tokens.push(c)
        else if c == " " or c == char(9) or c == char(10) or c == char(13) then
            // whitespace, skip
        else if c == ";" then
            // comment, skip to end of line
            while i < chars.len and chars[i] != char(10)
                i = i + 1
            end while
        else
            tok = c
            while i + 1 < chars.len and " ()[]{}""';`," .indexOf(chars[i+1]) == null
                i = i + 1
                tok = tok + chars[i]
            end while
            tokens.push(tok.trim)
        end if
        i = i + 1
    end while
    return tokens
end function


//  @Doc
//  Takes in a list
//  Reads tokens
//  Is recursive
//
readFromTokens = function(tokens)
    // We don't want an empty list
    if tokens.len == 0 then return lispError("Unexpected EOF")
    // We also dont want anything that is NOT a list
    if not @tokens isa list then return lispError("Not a list")
    token = tokens.pull
    
    // We encountered a symbol, parse it recursively
	if token == "(" then
    	L = []
    	while tokens.len > 0 and tokens[0] != ")"
        	item = readFromTokens(tokens)
        	if isError(@item) then return item
        	L.push(item)
    	end while
    	if tokens.len == 0 then return lispError("Unexpected EOF while reading list")
    	tokens.pull  // consume the )
    	return L

else if token == "[" then
    L = []
    while tokens.len > 0 and tokens[0] != "]"
        item = readFromTokens(tokens)
        if isError(@item) then return item
        L.push(item)
    end while
    if tokens.len == 0 then return lispError("Unexpected EOF while reading vector")
    tokens.pull  // consume the ]
    return ["array"] + L
    
    else if token == ")" then
		return lispError("Unexpected )")
    else if token == "]" then
        return lispError("Unexpected ]")

    // quote tokens for macroing around
	else if token == "'" then
    	inner = readFromTokens(tokens)
    	if isError(@inner) then return inner
    	return ["quote", inner]
	else if token == "`" then
    	inner = readFromTokens(tokens)
    	if isError(@inner) then return inner
    	return ["quasiquote", inner]
	else if token == "~@" then
    	inner = readFromTokens(tokens)
    	if isError(@inner) then return inner
    	return ["splice-unquote", inner]
	else if token == "~" then
    	inner = readFromTokens(tokens)
    	if isError(@inner) then return inner
    	return ["unquote", inner]
    // Return an atom, we can let the MiniScript type coercion do everything for us
    else 
		return atom(token)
    end if
end function

parse = function(code)
    tokens = tokenize(code)
    result = readFromTokens(tokens)
    if isError(@result) then return result
    if tokens.len > 0 then return lispError("Unexpected trailing tokens: " + tokens.join(" "))
    return result
end function

eval = function(exp, env)
	if @exp isa number then return exp
	if @exp == null then return null

    if @exp isa list then
        if exp.len == 0 then return exp

        first = exp[0]

        // handle special forms first

		if first == "quasiquote" then
    		return evalQuasiquote(exp[1], env)
		end if

		// Game interop
		if first isa string and first[0] == "." then
    		methodName = first[1:]
    		obj = eval(exp[1], env)
    		if obj == null then return lispError("null object in interop call ." + methodName)
			if isError(obj) then return obj // Check for errors!    

    		fn = @obj[methodName]
    
    		if not (@fn isa funcRef) then return @fn
    
    		args = []
    		if exp.len > 2 then
        		for i in range(2, exp.len-1)
					result = eval(exp[i], env)
					if isError(@result) then return @result
            		args.push(@result)
        		end for
    		end if

    		// pass obj as self, then spread remaining args
    		if args.len == 0 then return fn(@obj)
    		if args.len == 1 then return fn(@obj, args[0])
    		if args.len == 2 then return fn(@obj, args[0], args[1])
    		if args.len == 3 then return fn(@obj, args[0], args[1], args[2])
    		if args.len == 4 then return fn(@obj, args[0], args[1], args[2], args[3])
    		return lispError("Too many arguments for native method")
		end if
	
		if first == "array" then
    		result = []
    		if exp.len > 1 then
        		for i in range(1, exp.len-1)
            		val = eval(exp[i], env)
            		if isError(@val) then return val
            		result.push(val)
        		end for
    		end if
   		return result
		end if
	
		if first == "import" then
			path = exp[1]  // don't eval, take the raw token
    		// strip quotes if present
    		if path[0] == """" then path = path[1:-1]
    		//path = eval(exp[1], env)
    		hostComputer = get_shell.host_computer
			  fpath = get_abs_path(path)
    		f = hostComputer.File(fpath)
    		if f == null then return lispError("Error: file not found: " + path)
    		if f.is_binary then return lispError("Error: cannot import binary file: " + path)
    		contents = f.get_content
    		if contents == null then return lispError("Error: no read permission: " + path)
    		wrapped = "(do " + contents + ")"
			result = parse(wrapped)
			if isError(@result) then return result
    		return eval(result, env)
		end if
	
		if first == "set!" then
    		name = exp[1]
    		value = eval(exp[2], env)
			if isError(@value) then return value
    		return env.setExisting(name, value)
		end if

		if globalEnv.locals["macros"].hasIndex(first) then
    		macroFn = globalEnv.locals["macros"][first]  // no .get!
   			newExp = macroFn(exp[1:])
			res = eval(newExp, env)
			if isError(@res) then return res
    		return res
		end if

		if first == "defmacro" then
    		name = exp[1]
    		argNames = exp[2]
    		if argNames isa list and argNames.len > 0 and argNames[0] == "array" then
        		argNames = argNames[1:]
    		end if
    		body = exp[3]
    		closedEnv = env
    
			macroFn = function(forms)
    			__argNames = argNames  // capture locally
    			__body = body          // capture locally
    			__closedEnv = closedEnv
    			newEnv = makeEnv(__closedEnv)
    			if __argNames.len > 0 and forms.len > 0 then
        			for i in range(0, __argNames.len-1)
            			if __argNames[i] == "&" then
                			restName = __argNames[i+1]
                			if i >= forms.len then
                    		newEnv.set(restName, [])
                			else
                    			newEnv.set(restName, forms[i:])
                			end if
                			break
            			end if
            			if i >= forms.len then
                			newEnv.set(__argNames[i], null)
            			else
                			newEnv.set(__argNames[i], forms[i])
            			end if
        			end for
    			end if
    			return eval(__body, newEnv)
			end function
    
    		globalEnv.locals["macros"][name] = @macroFn
    		return name
		end if
	
		if first == "recur" then
    		args = []
    		if exp.len > 1 then
        		for i in range(1, exp.len-1)
        			result = eval(exp[i], env)
       				if isError(result) then return result
        			args.push(result)
        		end for
    		end if
    		return {"classID": "recur", "args": args}
		end if
	
		// try/catch special form in eval
		if first == "try" then
    		body = exp[1]
    		catchClause = exp[2]  // ["catch", ["array", "e"], handler]
    		result = eval(body, env)
    		if isError(@result) then
        		catchEnv = makeEnv(env)
        		catchBindings = catchClause[1]  // ["array", "e"]
        		if catchBindings isa list and catchBindings.len > 0 and catchBindings[0] == "array" then
            		catchBindings = catchBindings[1:]  // ["e"]
        		end if
        		catchEnv.set(catchBindings[0], result["message"])  // bind "e" to error message
        		return eval(catchClause[2], catchEnv)
    		end if
    		return result
		end if

		if first == "throw" then
    		msg = eval(exp[1], env)
    		if isError(msg) then return msg
    		return lispError(msg)
		end if
	
		if first == "apply" then
    		fn = eval(exp[1], env)
    		argList = eval(exp[2], env)
			if isError(@fn) then return fn
			if isError(@argList) then return argList
    		if not @argList isa list then return lispError("Apply requires a list as second argument")
    		isNative = globalEnv.natives.hasIndex(exp[1])
    		return callFunction(@fn, @argList, @exp[1], isNative)
		end if
	
		if first == "and" then
    		if exp.len == 1 then return true  // (and) with no args
    		result = true
			if exp.len > 1 then
    			for i in range(1, exp.len-1)
        			result = eval(exp[i], env)
					if isError(@result) then return result
        			if not result then return false  // short circuit
    			end for
			end if
    		return result
		end if

		if first == "or" then
    		if exp.len == 1 then return null  // (or) with no args
    		for i in range(1, exp.len-1)
        		result = eval(exp[i], env)
				if isError(@result) then return result
        		if result then return result  // short circuit, return truthy value
    		end for
    		return false
		end if

        if first == "quote" then
            return exp[1]
        end if

		if first == "let" then
    		bindings = exp[1]
    		if bindings isa list and bindings.len > 0 and bindings[0] == "array" then
        		bindings = bindings[1:]
    		end if
    		body = exp[2]
    		newEnv = makeEnv(env)
    		if bindings.len > 0 then
        		for i in range(0, bindings.len-1, 2)
            		value = eval(bindings[i+1], newEnv)
            		if isError(@value) then return value
            		newEnv.set(bindings[i], value)
        		end for
    		end if
    		return eval(body, newEnv)
		end if
	
		if first == "do" then
    		result = null
    		if exp.len > 1 then
        		for i in range(1, exp.len-1)
            		result = eval(exp[i], env)
					if isError(@result) then return result
        		end for
    		end if
    		return result
		end if
	
        if first == "if" then
            cond = eval(exp[1], env)
			if isError(@cond) then return cond
            if @cond then
                return eval(exp[2], env)
			else
    			if exp.len > 3 then return eval(exp[3], env)
    			return null
			end if
        end if

		if first == "def" or first == "define" then
    		name = exp[1]
    		value = eval(exp[2], env)
    		if isError(@value) then return value
    		env.set(name, value)
    		return value
		end if

		if first == "fn" then
    		params = exp[1]
    		if params isa list and params.len > 0 and params[0] == "array" then
        		params = params[1:]
    		end if
    		return {"classID": "fn", "args": params, "body": exp[2:], "env": env}
		end if
        
		// normal function call
		op = eval(first, env)
		if isError(@op) then return op
		args = []
		if exp.len > 1 then
    		for i in range(1, exp.len-1)
        		val = eval(exp[i], env)
				if isError(@val) then return val
            	args.push(@val)
    		end for
		end if
		isNative = globalEnv.natives.hasIndex(first)
		return callFunction(@op, @args, @first)
	else if @exp isa string then
    	// keywords are self-evaluating
    	if exp[0] == ":" then return exp
    	if exp[0] == """" then return exp[1:-1]  // string literal, strip quotes
    	return env.get(@exp)  // walks the chain, errors if not found
    else
        return exp
    end if
end function

// NATIVES.src

nativeFns = {
    "get_shell": @get_shell,
    "get_router": @get_router,
    "nslookup": @nslookup,
    "whois": @whois,
    "is_valid_ip": @is_valid_ip,
    "is_lan_ip": @is_lan_ip,
    "active_user": @active_user,
    "home_dir": @home_dir,
    "program_path": @program_path,
    "current_path": @current_path,
    "parent_path": @parent_path,
    "get_abs_path": @get_abs_path,
    "include_lib": @include_lib,
    "exit": @exit,
    "wait": @wait,
    "time": @time,
    "current_date": @current_date,
    "char": @char,
    "pi": @pi,
    "rnd": @rnd,
    "val": @val,
    "slice": @slice,
    "typeof": @typeof,
}

for kv in nativeFns
    globalEnv.locals[kv.key] = @kv.value
    globalEnv.natives[kv.key] = true
end for

// boot the stdlib
macros = "(import ./macros.lisp)"
stdlib = "(import ./stdlib.lisp)"
tests = "(import ./tests.lisp)"
eval(parse(macros), globalEnv)
eval(parse(stdlib), globalEnv)
eval(parse(tests), globalEnv)

// REPL
while true
    input = user_input("Clojette> ")
    if input == "exit" or input == "quit" or input == "q" then break
    result = eval(parse(input), globalEnv)
    print(result)
end while
