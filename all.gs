//   Copyright (C) 2026 lattiahirvio
//
//   This file is part of Clojette.
//   Clojette is licensed under GPLv3 with a special linking/importing exception.
//   See LICENSE for details.
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

clojette = {}

__runtimeTag__ = function
end function

clojette.lispError = function(msg=null, trace=null)
  if msg == null then msg = "Null"
  if trace == null then trace = []
  if not trace isa list then trace = ["invalid-trace-type: " + trace]
  return {"classID": "error", "__tag__": @__runtimeTag__, "message": msg, "trace": trace}
end function
// Environment setup, very cool.
clojette.makeEnv = function(outerEnv)
    e = {}
    e.locals = {}
    e.get = function(name)
      if self.locals.hasIndex(name) then return @self.locals[name]
      if outerEnv != null then return outerEnv.get(name)
      return clojette.lispError("Undefined in the env: " + name)
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
      return clojette.lispError("Cannot set! undefined variable: " + name)
	  end function
    return e
end function

clojette.bindArgs = function(argNames, params, baseEnv)
    newEnv = self.makeEnv(baseEnv)
    
    // No args expected
    if argNames.len == 0 then
      if params.len > 0 then
        return self.lispError("Wrong number of args: expected 0, got " + params.len)
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
            return self.lispError("Wrong number of args: expected at least " + restIdx + ", got " + params.len)
        end if
        for i in range(0, restIdx-1)
            // we can safely access restIdx, but since params can be of len 0, and if params is empty, accessing anything would crash; we do not want that, so we error.
            if params.len == 0 then return self.lispError("Cannot bind arguments for function [" + argNames.join(", ") + "]: expected at least 1 argument, got " + params.len)
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
            return self.lispError("Wrong number of args: expected " + argNames.len + ", got " + params.len)
        end if
        for i in range(0, argNames.len-1)
            newEnv.set(argNames[i], params[i])
        end for
    end if
    
    return newEnv
end function

//clojette.Env = {}
clojette.globalEnv = clojette.makeEnv(null)
// In the MiniScript bootstrap, before the REPL
clojette.globalEnv.locals["__recur_sentinel__"] = {"classID": "recur", "args": null}
clojette.globalEnv.locals["__gensym_counter__"] = 0
clojette.globalEnv.locals["macros"] = {}
clojette.globalEnv.locals["__namespaces__"] = {"user": {}}
clojette.globalEnv.locals["__current_ns__"] = "user"
clojette.globalEnv.locals["__ns_aliases__"] = {"user": {}}
clojette.globalEnv.natives = {}

// sentinels for the env, lets us use special forms from macros.
// Yes this is a non-ideal, but what can you do? 
// TODO: fix
clojette.globalEnv.locals["do"] = "do"
clojette.globalEnv.locals["if"] = "if"
clojette.globalEnv.locals["def"] = "def"
clojette.globalEnv.locals["fn"] = "fn"
clojette.globalEnv.locals["let"] = "let"
clojette.globalEnv.locals["quote"] = "quote"
clojette.globalEnv.locals["set!"] = "set!"

// Guards
clojette.realTypeof = function(anyObject)

    if @anyObject == null then return "null"

    // Custom fn object type
    if @anyObject isa map then
        if @anyObject.hasIndex("classID") and @anyObject["classID"] == "fn" then
            return "function"
        end if
    end if

    objectType = @anyObject * 0

    if objectType == "" then return "string"
    if objectType == [] then return "list"
    if @anyObject == {} then return "map"

    for i in @anyObject
        return "map"
    end for

    if objectType == null then return "function"

    return "number"
end function

clojette.checkArity = function(sig, argc)

    if sig == "*" then return true

    if sig[sig.len-1] == "+" then
        min = val(sig[:-1])
        return argc >= min
    end if

    dash = sig.indexOf("-")

    if dash != null then
        min = val(sig[:dash])
        max = val(sig[dash+1:])
        return argc >= min and argc <= max
    end if

    return argc == val(sig)
end function

clojette.matchesType = function(expected, actual)
    if expected isa list then
        for t in expected
            if t == "all" then return true
            if t == "any" then
              if actual == "null" then return false
              if actual == "function" then return false
              return true
            end if
            if t == actual then return true
        end for
        return false
    end if

    if expected == "any" then
        if actual == "null" then return false
        if actual == "function" then return false
        return true
    end if

    if expected == "all" then return true
    return expected == actual
end function

clojette.guard = function(arity, types, args, argname = null, message=null)
    argc = args.len

    if not self.checkArity(arity, argc) then
        if argname != null then return self.lispError(argname + ": invalid arity")
        return self.lispError("invalid arity")
    end if

    // empty args, so there is a function that can take any amount of args and got 0 args... so we don't want to really do anything?
    // if a function wants more than 1 argument, it should require 1+
    if args.len == 0 then 
        return null
    end if

    for i in range(0, argc-1)
        actual = self.realTypeof(args[i])

        if i >= types.len then
            expected = types[types.len-1]
        else
            expected = types[i]
        end if

        if not self.matchesType(expected, actual) then
            if message != null then return self.lispError(message)
            if argname != null then return self.lispError(argname + ": expected " + str(expected) + ", got " + actual)
            return self.lispError("expected " + str(expected) + ", got " + actual)
        end if
    end for

    return null
end function

//
// Clojette Builtins - MiniScript host layer
//
clojette.globalEnv.locals["gensym"] = function(args)
    prefix = "G__"
    if args.len > 0 then prefix = args[0]
    return clojette.gensym(prefix)
end function

// guard!
clojette.globalEnv.locals["guard"] = function(args)
    err = clojette.guard("3-5", ["string", "list", "list", "string"], args, "guard")
    if clojette.isError(err) then return err

    types = args[0]
    values = args[1]
    arguments = args[2]
    name = null
    msg = null
    if args.len > 3 then name = args[3] 
    if args.len > 4 then msg = args[4] 

    return clojette.guard(types, values, arguments, name, msg)
end function

clojette.globalEnv.locals["real-type?"] = function(args)
    err = clojette.guard("1", ["any"], args, "real-type?")
    if clojette.isError(err) then return err

    return clojette.realTypeof(args[0])
end function

// Arithmetic
clojette.globalEnv.locals["+"] = function(args)
    err = clojette.guard("*", [["number", "string", "list", "map"]], args, "+")
    if clojette.isError(err) then return err
    
    sum = 0
    if args.len == 0 then return 0
    for i in range(0, args.len-1)
        sum = sum + args[i]
    end for
    return sum
end function

clojette.globalEnv.locals["-"] = function(args)
    err = clojette.guard("1+", [["number", "string"]], args, "-")
    if clojette.isError(err) then return err

    if args.len == 0 then return clojette.lispError("- requires at least 1 argument")
    if args.len == 1 then return -args[0]
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            result = result - args[i]
        end for
    end if
    return result
end function

clojette.globalEnv.locals["*"] = function(args)
    err = clojette.guard("*", [["number", "string", "list"]], args, "*")
    if clojette.isError(err) then return err

    prod = 1
    if args.len == 0 then return 1
    for i in range(0, args.len-1)
        prod = prod * args[i]
    end for
    return prod
end function

clojette.globalEnv.locals["/"] = function(args)
    err = clojette.guard("1+", [["number", "string", "list"]], args, "/")
    if clojette.isError(err) then return err
      
    if args.len == 0 then return clojette.lispError("/ requires at least 1 argument")
    if args.len == 1 then
        if args[0] == 0 then return clojette.lispError("Division by zero")
        return 1 / args[0]
    end if
    result = args[0]
    for i in range(1, args.len-1)
        if args[i] == 0 then return clojette.lispError("Division by zero")
        result = result / args[i]
    end for
    return result
end function

clojette.globalEnv.locals["%"] = function(args)
    err = clojette.guard("2", ["number"], args, "%")
    if clojette.isError(err) then return err

    if args[1] == 0 then return clojette.lispError("Modulo by zero")
    return args[0] % args[1]
end function

clojette.globalEnv.locals["mod"] = function(args)
    err = clojette.guard("2", ["number"], args, "mod")
    if clojette.isError(err) then return err

    if args[1] == 0 then return clojette.lispError("Modulo by zero")
    return args[0] % args[1]
end function

clojette.globalEnv.locals["**"] = function(args)
    err = clojette.guard("2", ["number"], args, "**")
    if clojette.isError(err) then return err
    return args[0] ^ args[1]
end function

clojette.globalEnv.locals["quot"] = function(args)
    err = clojette.guard("2", ["number"], args, "quot")
    if clojette.isError(err) then return err

    if args.len != 2 then return clojette.lispError("quot requires exactly 2 arguments")
    if args[1] == 0 then return clojette.lispError("Division by zero")
    return floor(args[0] / args[1])
end function

// Comparison
clojette.globalEnv.locals["="] = function(args)
    err = clojette.guard("2+", ["all"], args)
    if clojette.isError(err) then return err

    for i in range(1, args.len-1)
        if args[i] != args[0] then return false
    end for
    return true
end function

clojette.globalEnv.locals["not="] = function(args)
    err = clojette.guard("2", ["all"], args)
    if clojette.isError(err) then return err
    return args[0] != args[1]
end function

clojette.globalEnv.locals["<"] = function(args)
    err = clojette.guard("2+", ["all"], args)
    if clojette.isError(err) then return err
    for i in range(1, args.len-1)
        if args[i-1] >= args[i] then return false
    end for
    return true
end function

clojette.globalEnv.locals[">"] = function(args)
    err = clojette.guard("2+", ["all"], args)
    if clojette.isError(err) then return err
    for i in range(1, args.len-1)
        if args[i-1] <= args[i] then return false
    end for
    return true
end function

clojette.globalEnv.locals["<="] = function(args)
    err = clojette.guard("2+", ["all"], args)
    if clojette.isError(err) then return err
    for i in range(1, args.len-1)
        if args[i-1] > args[i] then return false
    end for
    return true
end function

clojette.globalEnv.locals[">="] = function(args)
    err = clojette.guard("2+", ["all"], args)
    if clojette.isError(err) then return err
    for i in range(1, args.len-1)
        if args[i-1] < args[i] then return false
    end for
    return true
end function

clojette.globalEnv.locals["not"] = function(args)
    err = clojette.guard("1", ["all"], args)
    if clojette.isError(err) then return err
    if args.len != 1 then return clojette.lispError("not requires exactly 1 argument")
    return not args[0]
end function

// List operations
clojette.globalEnv.locals["list"] = function(args)
  err = clojette.guard("*", ["any"], args, "list")
  if clojette.isError(err) then return err
  return [] + args
end function

clojette.globalEnv.locals["car"] = function(args)
    err = clojette.guard("1", ["list"], args, "car")
    if clojette.isError(err) then return err

    lst = args[0]
    if lst == null or lst.len == 0 then return clojette.lispError("car called on empty list")
    return lst[0]
end function

clojette.globalEnv.locals["cdr"] = function(args)
    err = clojette.guard("1", ["list"], args, "cdr")
    if clojette.isError(err) then return err

    lst = args[0]
    if len(lst) <= 1 then return []
    return lst[1:]
end function

clojette.globalEnv.locals["cons"] = function(args)
    err = clojette.guard("2", ["any", ["list", "null"]], args, "cons")
    if args[1] == null then return [args[0]]
    return [args[0]] + args[1]
end function

clojette.globalEnv.locals["first"] = function(args)
    err = clojette.guard("1", ["list"], args, "first")
    if clojette.isError(err) then return err

    lst = args[0]
    if lst == null or lst.len == 0 then return null
    return lst[0]
end function

clojette.globalEnv.locals["second"] = function(args)
    err = clojette.guard("1", ["list"], args, "second")
    if clojette.isError(err) then return err

    lst = args[0]
    if lst == null then return null
    if lst.len < 2 then return null // clojette.lispError("Self needs a list longer than 1!")
    return lst[1]
end function

clojette.globalEnv.locals["rest"] = function(args)
    err = clojette.guard("1", ["list"], args, "rest")
    if clojette.isError(err) then return err

    lst = args[0]
    if lst.len <= 1 then return []
    return lst[1:]
end function

clojette.globalEnv.locals["conj"] = function(args)
    err = clojette.guard("2+", [["list", "null"], "any"], args, "conj")
    if clojette.isError(err) then return err

    result = args[0]
    if result == null then result = []
    for i in range(1, args.len-1)
      result = result + [args[i]]
    end for
    return result
end function

clojette.globalEnv.locals["concat"] = function(args)
    err = clojette.guard("*", [["list", "null"]], args, "concat")
    if clojette.isError(err) then return err

    result = []
    if args.len == 0 then return result
    for i in range(0, args.len-1)
        if args[i] != null then result = result + args[i]
    end for
    return result
end function

clojette.globalEnv.locals["empty?"] = function(args)
    err = clojette.guard("1",[["list", "string", "map", "null"]], args, "empty?")
    if clojette.isError(err) then return err

    lst = args[0]
    if lst == null then return true
    return lst.len == 0
end function

clojette.globalEnv.locals["count"] = function(args)
    err = clojette.guard("1", [["list", "string", "map"]], args, "count")
    if clojette.isError(err) then return err
    return args[0].len
end function

clojette.globalEnv.locals["list?"] = function(args)
    err = clojette.guard("1", ["any"], args, "list?")
    if clojette.isError(err) then return err
    return args[0] isa list
end function

clojette.globalEnv.locals["nth"] = function(args)
    err = clojette.guard("2", ["list", "number"], args, "nth")
    if clojette.isError(err) then return err

    lst = args[0]
    n = args[1]
    if lst == null or n >= lst.len then return clojette.lispError("nth index out of bounds")
    return lst[n]
end function

clojette.globalEnv.locals["get"] = function(args)
    err = clojette.guard("2-3", [["list", "map", "string", "null"], "any", "any"], args, "get")
    if clojette.isError(err) then return err

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
clojette.globalEnv.locals["hash-map"] = function(args)
    err = clojette.guard("*", ["any"], args, "hash-map")
    if clojette.isError(err) then return err

    result = {}
    if args.len == 0 then return result
    if args.len % 2 != 0 then return clojette.lispError("hash-map requires even number of arguments")
    for i in range(0, args.len-1, 2)
        result[args[i]] = @args[i+1]
    end for
    return result
end function

clojette.globalEnv.locals["assoc"] = function(args)
    err = clojette.guard("3+", [["map", "null"], "any"], args, "assoc")
    if clojette.isError(err) then return err
    result = {}
    if args[0] != null then
        for kv in args[0]
            result[kv.key] = @kv.value
        end for
    end if
    for i in range(1, args.len-1, 2)
        result[args[i]] = @args[i+1]
    end for
    return result
end function

clojette.globalEnv.locals["dissoc"] = function(args)
    err = clojette.guard("2+", ["map", "any"], args, "dissoc")
    if clojette.isError(err) then return err

    result = {}
    for kv in args[0]
        result[kv.key] = @kv.value
    end for
    for i in range(1, args.len-1)
        result.remove(args[i])
    end for
    return result
end function

clojette.globalEnv.locals["keys"] = function(args)
    err = clojette.guard("1", [["map", "null"]], args, "keys")
    if clojette.isError(err) then return err

    if args[0] == null then return []
    result = []
    for kv in args[0]
        result.push(kv.key)
    end for
    return result
end function

clojette.globalEnv.locals["vals"] = function(args)
    err = clojette.guard("1", [["map", "null"]], args, "vals")
    if clojette.isError(err) then return err

    if args[0] == null then return []
    result = []
    for kv in args[0]
        result.push(@kv.value)
    end for
    return result
end function

clojette.globalEnv.locals["map?"] = function(args)
    err = clojette.guard("1", ["all"], args, "map?")
    if clojette.isError(err) then return err
    
    if args.len != 1 then return clojette.lispError("map? requires exactly 1 argument")
    return args[0] isa map
end function

clojette.globalEnv.locals["contains?"] = function(args)
    err = clojette.guard("2", [["map", "list", "string", "null"], "any"], args, "contains")
    if args.len != 2 then return clojette.lispError("contains? requires exactly 2 arguments")
    if args[0] == null then return false
    return args[0].hasIndex(args[1])
end function

// Type checks
clojette.globalEnv.locals["number?"] = function(args)
    err = clojette.guard("1", ["all"], args, "number?")
    if clojette.isError(err) then return err
    return args[0] isa number
end function

clojette.globalEnv.locals["string?"] = function(args)
    err = clojette.guard("1", ["all"], args, "string?")
    if clojette.isError(err) then return err
    return args[0] isa string
end function

clojette.globalEnv.locals["null?"] = function(args)
    err = clojette.guard("1", ["all"], args, "null?")
    if clojette.isError(err) then return err
    return args[0] == null
end function

clojette.globalEnv.locals["fn?"] = function(args)
    err = clojette.guard("1", ["all"], args, "fn?")
    if clojette.isError(err) then return err
    if args[0] isa funcRef then return true
    return args[0] isa map and args[0].hasIndex("classID") and args[0]["classID"] == "fn"
end function

clojette.globalEnv.locals["true?"] = function(args)
    err = clojette.guard("1", ["all"], args, "true?")
    if clojette.isError(err) then return err
    return args[0] == true
end function

clojette.globalEnv.locals["false?"] = function(args)
    err = clojette.guard("1", ["all"], args, "false?")
    if clojette.isError(err) then return err
    return args[0] == false
end function

// Math
clojette.globalEnv.locals["floor"] = function(args)
    err = clojette.guard("1", ["number"], args)
    if clojette.isError(err) then return err
    return floor(args[0])
end function

clojette.globalEnv.locals["ceil"] = function(args)
    err = clojette.guard("1", ["number"], args)
    if clojette.isError(err) then return err
    return ceil(args[0])
end function

clojette.globalEnv.locals["round"] = function(args)
    err = clojette.guard("1", ["number"], args)
    if clojette.isError(err) then return err
    return round(args[0])
end function

clojette.globalEnv.locals["abs"] = function(args)
    err = clojette.guard("1", ["number"], args)
    if clojette.isError(err) then return err
    return abs(args[0])
end function

clojette.globalEnv.locals["sqrt"] = function(args)
    err = clojette.guard("1", ["number"], args)
    if clojette.isError(err) then return err
    return sqrt(args[0])
end function

clojette.globalEnv.locals["max"] = function(args)
    err = clojette.guard("1+", ["number"], args)
    if clojette.isError(err) then return err
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            if args[i] > result then result = args[i]
        end for
    end if
    return result
end function

clojette.globalEnv.locals["min"] = function(args)
    err = clojette.guard("1+", ["number"], args)
    if clojette.isError(err) then return err
    result = args[0]
    if args.len > 1 then
        for i in range(1, args.len-1)
            if args[i] < result then result = args[i]
        end for
    end if
    return result
end function

// String operations
clojette.globalEnv.locals["str"] = function(args)
    err = clojette.guard("*", ["all"], args)
    if clojette.isError(err) then return err
    result = ""
    if args.len == 0 then return result
    for i in range(0, args.len-1)
        result = result + str(args[i])
    end for
    return result
end function

clojette.globalEnv.locals["split"] = function(args)
    err = clojette.guard("2", ["string", "string"], args)
    if clojette.isError(err) then return err
    return args[0].split(args[1])
end function

clojette.globalEnv.locals["join"] = function(args)
    err = clojette.guard("2", ["list", "string"], args)
    if clojette.isError(err) then return err
    return args[0].join(args[1])
end function

clojette.globalEnv.locals["trim"] = function(args)
    err = clojette.guard("1", ["string"], args)
    if clojette.isError(err) then return err
    return args[0].trim
end function

clojette.globalEnv.locals["index-of"] = function(args)
    err = clojette.guard("2", ["string", "string"], args)
    if clojette.isError(err) then return err
    if args.len != 2 then return clojette.lispError("index-of requires exactly 2 arguments")
    return args[0].indexOf(args[1])
end function

clojette.globalEnv.locals["subs"] = function(args)
    err = clojette.guard("2-3", ["string", "number", "number"], args)
    if clojette.isError(err) then return err
    if args.len == 2 then return args[0][args[1]:]
    return args[0][args[1]:args[2]]
end function

clojette.globalEnv.locals["upper-case"] = function(args)
    err = clojette.guard("1", ["string"], args)
    if clojette.isError(err) then return err
    return args[0].upper
end function

clojette.globalEnv.locals["lower-case"] = function(args)
    err = clojette.guard("1", ["string"], args)
    if clojette.isError(err) then return err
    return args[0].lower
end function

clojette.globalEnv.locals["replace"] = function(args)
    err = clojette.guard("3", ["string", "string", "string"], args)
    if clojette.isError(err) then return err
    haystack = args[0]
    needle = args[1]
    replacement = args[2]
    if needle == "" then return clojette.lispError("replace: needle cannot be empty")
    return haystack.replace(needle, replacement)
end function

// I/O
clojette.globalEnv.locals["println"] = function(args)
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

clojette.globalEnv.locals["user-input"] = function(args)
    if args.len > 0 then return user_input(args[0])
    return user_input("")
end function

// Apply - needed for higher order functions
clojette.globalEnv.locals["apply"] = function(args)
    err = clojette.guard("2", ["function", "list"], args)
    if clojette.isError(err) then return err
    fn = @args[0]
    argList = args[1]
    return callFunction(@fn, argList, "apply")
end function

clojette.globalEnv.locals["take-keys"] = function(args)
    err = clojette.guard("1", ["any"], args)
    if clojette.isError(err) then return err

    bindings = args[0]
    if bindings isa list and len(bindings) > 0 and bindings[0] == "array" then
        bindings = bindings[1:]
    end if
    result = []
    for i in range(0, len(bindings)-1, 2)
        result.push(bindings[i])
    end for
    return result
end function

clojette.globalEnv.locals["take-vals"] = function(args)
    err = clojette.guard("1", ["any"], args)
    if clojette.isError(err) then return err

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
clojette.globalEnv.locals["true"] = true
clojette.globalEnv.locals["false"] = false
clojette.globalEnv.locals["null"] = null
clojette.globalEnv.locals["nil"] = null

// helpers
clojette.atom = function(token)
  if @token isa map then return self.lispError("Tried to give a map to atom()...") 
	
  // We dereference the token to not invoke anything by accident
  if @token isa number then return token
	if @token isa funcRef then return self.lispError("Tried evaluating funcRef as an atom?")
	
  // Return full string literal
  if token[0] == """" then return token
  num = token.val
  if str(num) == token then return num
  return token
end function

// We can check if a given result is an error; we want error handling
clojette.isError = function(val)
  if not @val isa map then return false
  // We know that the op is a map, and potentially is an error; safe to handle without deref 
  if @val.hasIndex("classID") and @val["classID"] == "error" then
    if not @val.hasIndex("__tag__") then return false
    return @val["__tag__"] == @__runtimeTag__
  end if
	return false
end function

clojette.addTrace = function(err, frame)
    if not err.hasIndex("trace") then err["trace"] = []
    err["trace"].push(frame)
    return err
end function

clojette.isRuntimeObject = function(val)
    if not @val isa map then return false
    if not val.hasIndex("__tag__") then return false
    return @val["__tag__"] == @__runtimeTag__
end function

clojette.gensym = function(prefix="G__")
    counter = self.globalEnv.locals["__gensym_counter__"] + 1
    self.globalEnv.locals["__gensym_counter__"] = counter
    return prefix + str(counter)
end function

clojette.macroexpand_1 = function(exp)
    macromap = self.globalEnv.locals["macros"]

    if not (exp isa list) then
        return exp
        //return self.lispError("macroexpand requires a list!")
    end if

    // walk each item in the expression
    for item in exp
        // recurse into nested lists
        if item isa list then
            expanded = self.macroexpand_1(item)

            // if recursion expanded something, return immediately
            if expanded != null then
                return expanded
            end if
        else
            // check if symbol is a macro
            if macromap.hasIndex(item) then
                macroFn = macromap[item]

                // pass args after macro name
                return macroFn(exp[1:])
            end if
        end if
    end for

    return null
end function

// There was an error here, where we were trying to check if op was a funcRef
// that check was not dereferenced and we called it directly -_-
// Lesson learned, always deref your functions
clojette.callFunction = function(op, args, name, isNative=false)
    if self.isError(@op) then return @op
    
    // User-defined Clojette fn
    if @op isa map then
        if op.hasIndex("classID") and op["classID"] == "fn" then
            while true
                newEnv = self.bindArgs(op["args"], args, op["env"])
                if self.isError(@newEnv) then return newEnv
                result = null
                for bodyExpr in op["body"]
                    result = self.eval(bodyExpr, newEnv)
                    if self.isError(@result) then return self.addTrace(@result, "in " + name)
                end for
                // check if recur was signalled
                if result isa map and result.hasIndex("classID") and result["classID"] == "recur" then
                    args = result["args"]
                else
                    return result
                end if
            end while
        end if
    end if

    // funcRef - either stdlib or native MiniScript
    if @op isa funcRef or typeof(@op) == "function" then // its a native
        if isNative then
            if args.len == 0 then return @op()
            if args.len == 1 then return @op(args[0])
            if args.len == 2 then return @op(args[0], args[1])
            if args.len == 3 then return @op(args[0], args[1], args[2])
            if args.len == 4 then return @op(args[0], args[1], args[2], args[3])
            if args.len == 5 then return @op(args[0], args[1], args[2], args[3], args[4])
            return self.lispError("Native functions support at most 5 arguments")
        else
            return op(@args)
        end if
    end if
    
    return self.lispError("Not a function: " + name)
end function

clojette.evalQuasiquote = function(exp, env, gensyms=null)
    if gensyms == null then gensyms = {}

    if exp isa string and exp.len > 0 and exp[0] != """" and exp[exp.len-1] == "#" then
      if not gensyms.hasIndex(exp) then
        gensyms[exp] = self.gensym(exp[0:exp.len-1] + "__")
      end if
    
      return gensyms[exp]
    end if

    // not a list, just return it as-is (like quote)
    if not @exp isa list then return exp
    // empty list
    if @exp.len == 0 then return exp
    
    // unquote: evaluate and return
    if exp[0] == "unquote" then
		result = self.eval(exp[1], env)
    if self.isError(@result) then return result
		return result
    end if
    
    // walk the list, handling splice-unquote
    result = []
    // we dont need to check bounds because it returns earlier if it is not
    for i in range(0, exp.len-1)
        item = exp[i]
        if item isa list and item.len > 0 and item[0] == "splice-unquote" then
          spliced = self.eval(item[1], env)
          if self.isError(@spliced) then return spliced
          if not @spliced isa list then return self.lispError("splice-unquote requires a list, got: " + typeof(@spliced))
            if spliced.len > 0 then
              for j in range(0, spliced.len-1)
                result.push(spliced[j])
              end for
            end if
        else
    	    item = self.evalQuasiquote(@item, env, gensyms)
    		  if self.isError(@item) then return item
    		  result.push(@item)
        end if
    end for
    return result
end function

// Convert a string of characters into a list of tokens
clojette.tokenize = function(chars)
    tokens = []
    //if tokens.len == 0 then return self.lispError("Unexpected EOF")
    //if self.isError(tokens[0]) then return tokens.pull  // propagate tokenizer errors
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
        	tokens.push(self.lispError("Unterminated string literal: " + tok))
        	return tokens
    		end if
    		tok = tok + """"
    		tokens.push(tok)
        else if c == "(" or c == ")" or c == "[" or c == "]" or c == "{" or c == "}" then
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
//  This is the reader. The reader takes in a list of tokens
//  and then reads those tokens recursively. The reader 
//  mutates the tokens for some syntax sugar.
//  Macros depend on this, for example.
//
clojette.readFromTokens = function(tokens)
    // We don't want an empty list
    if tokens.len == 0 then return self.lispError("Unexpected EOF")
    // We also dont want anything that is NOT a list
    if not @tokens isa list then return self.lispError("Not a list")
    token = tokens.pull
    
    // We encountered a symbol, parse it recursively
	  if token == "(" then
    	L = []
    	while tokens.len > 0 and tokens[0] != ")"
        item = self.readFromTokens(tokens)
        if self.isError(@item) then return item
        L.push(item)
    	end while
    	if tokens.len == 0 then return self.lispError("Unexpected EOF while reading list")
    	tokens.pull  // consume the )
    	return L

    // Handle arrays
    else if token == "[" then
      L = []
      while tokens.len > 0 and tokens[0] != "]"
          item = self.readFromTokens(tokens)
          if self.isError(@item) then return item
          L.push(item)
      end while
      if tokens.len == 0 then return self.lispError("Unexpected EOF while reading vector")
      tokens.pull  // consume the ]
      return ["array"] + L

    // Handle hash maps
    else if token == "{" then
      L = []
      while tokens.len > 0 and tokens[0] != "}"
          item = self.readFromTokens(tokens)
          if self.isError(@item) then return item
          L.push(item)
      end while
      if tokens.len == 0 then return self.lispError("Unexpected EOF while reading map")
      tokens.pull  // consume the }
      return ["hash-map"] + L
      
      else if token == ")" then
  		return self.lispError("Unexpected )")
      else if token == "]" then
        return self.lispError("Unexpected ]")
      else if token == "}" then
        return self.lispError("Unexpected }")

    // Handle syntax sugar for anonymous functions...
    else if token == "#" then
      if tokens[0] != "(" then
        return self.lispError("Expected #(...) form")
      end if
    
      tokens.pull
    
      body = []
      while tokens.len > 0 and tokens[0] != ")"
        body.push(self.readFromTokens(tokens))
      end while
    
      if tokens.len == 0 then
        return self.lispError("Unterminated #(...)")
      end if
    
      tokens.pull
    
      return ["fn",["array", "&", "args"], ["do"] + body]
      // In the future, if I need other # forms, they are added here.

  
    // quote tokens for macroing around
  	else if token == "'" then
    	inner = self.readFromTokens(tokens)
    	if self.isError(@inner) then return inner
      return ["quote", inner]
  	else if token == "`" then
      inner = self.readFromTokens(tokens)
      if self.isError(@inner) then return inner
      return ["quasiquote", inner]
  	else if token == "~@" then
      inner = self.readFromTokens(tokens)
      if self.isError(@inner) then return inner
      return ["splice-unquote", inner]
  	else if token == "~" then
      inner = self.readFromTokens(tokens)
      if self.isError(@inner) then return inner
      return ["unquote", inner]
    // DONE: Fix gensym because this is NOT working
    //else if token[token.len-1] == "#" then // Runtime gensym?
      //return ["gensym", token[0:token.len - 1]]
    // Return an atom, we can let the MiniScript type coercion do everything for us
    else 
  	return self.atom(token)
  end if
end function
// @Doc
// This is the parser
// It takes in code, tokenizes it, and returns te AST.
// 
clojette.parse = function(code)
    tokens = self.tokenize(code)
    result = self.readFromTokens(tokens)
    if self.isError(@result) then return result
    if tokens.len > 0 then return self.lispError("Unexpected trailing tokens: " + tokens.join(" "))
    return result
end function

clojette.eval = function(exp, env)
	if @exp isa number then return exp
	if @exp == null then return null

  if @exp isa list then
    if exp.len == 0 then return exp

    first = exp[0]

    // handle special forms first
		if first == "quasiquote" then
    	return self.evalQuasiquote(exp[1], env)
		end if

		// Game interop
		if first isa string and first[0] == "." then
    	methodName = first[1:]
    	obj = self.eval(exp[1], env)
    	if @obj == null then return self.lispError("null object in interop call ." + methodName)
      if self.isError(@obj) then return self.addTrace(@obj, "in " + first) // Check for errors!    

    		fn = @obj[methodName]

    		if not (@fn isa funcRef) then return @fn

    		args = []
    		if exp.len > 2 then
        	for i in range(2, exp.len-1) // bounds checked!
          result = self.eval(exp[i], env)
          if self.isError(@result) then return @result
            args.push(@result)
        	end for
    		end if

    		// pass obj as self, then spread remaining args
    		if args.len == 0 then return fn(@obj)
    		if args.len == 1 then return fn(@obj, args[0])
    		if args.len == 2 then return fn(@obj, args[0], args[1])
    		if args.len == 3 then return fn(@obj, args[0], args[1], args[2])
    		if args.len == 4 then return fn(@obj, args[0], args[1], args[2], args[3])
    		if args.len == 5 then return fn(@obj, args[0], args[1], args[2], args[3], args[4])
    		return self.lispError("Too many arguments for native method")
    end if

    if first == "array" then
    		result = []
    		if exp.len > 1 then
        		for i in range(1, exp.len-1) // bounds are checked
            		val = self.eval(exp[i], env)
            		if self.isError(@val) then return val
            		result.push(val)
        		end for
    		end if
      return result
    end if

    if first == "import" then
      path = exp[1]  // don't eval, take the raw token
    	// strip quotes if present
    	if path[0] == """" then path = path[1:-1]

    	//path = self.eval(exp[1], env)
    	hostComputer = get_shell.host_computer
      fpath = get_abs_path(path)
    	f = hostComputer.File(fpath)
    	
      if f == null then return self.lispError("Error: file not found: " + path)
    	if f.is_binary then return self.lispError("Error: cannot import binary file: " + path)
    	contents = f.get_content
    	if contents == null then return self.lispError("Error: no read permission: " + path)
    	wrapped = "(do " + contents + ")"
      result = self.parse(wrapped)
      if self.isError(@result) then return result
    	return self.eval(result, env)
		end if
	
		if first == "set!" then
    	name = exp[1]
    	value = self.eval(exp[2], env)
			if self.isError(@value) then return value
    		return env.setExisting(name, value)
		end if

		if self.globalEnv.locals["macros"].hasIndex(first) then
    	macroFn = self.globalEnv.locals["macros"][first]  // no .get!
   		newExp = macroFn(exp[1:])
			res = self.eval(newExp, env)
			if self.isError(@res) then return res
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
    			newEnv = clojette.makeEnv(__closedEnv)
    			if __argNames.len > 0 and forms.len > 0 then
        			for i in range(0, __argNames.len-1) // bounds are checked
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
    			return clojette.eval(__body, newEnv)
			end function
    
    		self.globalEnv.locals["macros"][name] = @macroFn
    		return name
		end if
	
		if first == "recur" then
    		args = []
    		if exp.len > 1 then
        		for i in range(1, exp.len-1) // bounds are checked!
        			result = self.eval(exp[i], env)
       				if self.isError(result) then return result
        			args.push(result)
        		end for
    		end if
    		return {"classID": "recur", "args": args}
		end if
	
		// try/catch special form in eval
		if first == "try" then
    		body = exp[1]
    		result = self.eval(body, env)
    		if self.isError(@result) then
    		    if exp.len < 3 then return result
    		    catchClause = exp[2]
        		catchBindings = catchClause[1]
        		if catchBindings isa list and catchBindings.len > 0 and catchBindings[0] == "array" then
        		    catchBindings = catchBindings[1:]
        		end if
        		catchEnv = self.makeEnv(env)
        		if catchBindings.len > 0 then
                if result.hasIndex("trace") then catchEnv.set(catchBindings[0], {"message": result["message"], "trace": result["trace"]}) 
        		    else 
                  catchEnv.set(catchBindings[0], {"message": result["message"], "trace": []})
        		end if
        		return self.eval(catchClause[2], catchEnv)
    		end if
    		return result
		end if

		if first == "throw" then
    		msg = self.eval(exp[1], env)
    		if self.isError(msg) then return msg
        trace = null
        if exp.len >= 3 then 
          trace = self.eval(exp[2], env)
          if self.isError(trace) then return trace
        end if
        if msg isa map then return self.lispError(msg["message"], msg["trace"])
    		return self.lispError(msg, trace)
		end if
	
		if first == "apply" then
    		fn = self.eval(exp[1], env)
    		argList = self.eval(exp[2], env)
			  if self.isError(@fn) then return fn
			  if self.isError(@argList) then return argList
    		if not @argList isa list then return self.lispError("Apply requires a list as second argument")
    		isNative = self.globalEnv.natives.hasIndex(exp[1])
        res = self.callFunction(@fn, @argList, @exp[1], isNative)
        if self.isError(@res) then return self.addTrace(@res, "in " + first) 
    		return res
		end if
	
		if first == "and" then
    		result = true
			  if exp.len > 1 then
    			for i in range(1, exp.len-1) // bounds are checked
        		result = self.eval(exp[i], env)
					if self.isError(@result) then return result
					if not result then return result
    			end for
			  end if
    		return result
		end if

		if first == "or" then
    		if exp.len == 1 then return null  // (or) with no args
    		for i in range(1, exp.len-1)
        		result = self.eval(exp[i], env)
				if self.isError(@result) then return result
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
    		newEnv = self.makeEnv(env)
    		if bindings.len > 0 then
        		for i in range(0, bindings.len-1, 2)
            		value = self.eval(bindings[i+1], newEnv)
            		if self.isError(@value) then return value
            		newEnv.set(bindings[i], value)
        		end for
    		end if
    		return self.eval(body, newEnv)
		end if
	
		if first == "do" then
    		result = null
    		if exp.len > 1 then
        		for i in range(1, exp.len-1)
            		result = self.eval(exp[i], env)
					if self.isError(@result) then return result
        		end for
    		end if
    		return result
		end if
	
      if first == "if" then
        cond = self.eval(exp[1], env)
			  if self.isError(@cond) then return cond
        if @cond then
          return self.eval(exp[2], env)
			  else
    		  if exp.len > 3 then return self.eval(exp[3], env)
    			return null
			  end if
      end if

		if first == "ns" then
			nsName = exp[1]
    	if nsName isa list then nsName = exp[1][1]  // handle quoted ns names
   		namespaces = self.globalEnv.locals["__namespaces__"]
    	if not namespaces.hasIndex(nsName) then
        namespaces[nsName] = {}
        self.globalEnv.locals["__ns_aliases__"][nsName] = {}
    	end if
    	self.globalEnv.locals["__current_ns__"] = nsName
    	return nsName
		end if

		if first == "def" or first == "define" then
    	name = exp[1]
    	value = self.eval(exp[2], env)
    	if self.isError(@value) then return value
			currentNs = self.globalEnv.locals["__current_ns__"]
			self.globalEnv.locals["__namespaces__"][currentNs][name] = @value
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
		op = self.eval(first, env)
		if self.isError(@op) then return op
		args = []
		if exp.len > 1 then
    	for i in range(1, exp.len-1)
      	val = self.eval(exp[i], env)
			  if self.isError(@val) then return val
        args.push(@val)
    	end for
		end if

    // Handle syntax for (:x map), basically looks up stuff from maps using keywords.
    // Very Clojure-esque.
    if first[0] == ":" and args[0] isa map then
      if self.isError(args[0]) then return args[0] // Check if 2nd element is an error, and return early... Is this more efficient? No clue.
      if args[0].hasIndex(first) then return args[0][first] 
      // Map doesnt have index for :x, so we perform a lookup for x
      if args[0].hasIndex(first[1:]) then return args[0][first[1:]]
      return self.lispError("Key " + first + " not found from map " + args[0])
    end if

		isNative = self.globalEnv.natives.hasIndex(first)
    res = self.callFunction(@op, @args, @first, isNative)
    if self.isError(@res) then return self.addTrace(@res, " in " + first)
		return res
	  
    else if @exp isa string then
    	// keywords are self-evaluating
    	if exp[0] == ":" then return exp
    	if exp[0] == """" then return exp[1:-1]  // string literal, strip quotes

		if exp.indexOf("/") != null then
    	parts = exp.split("/")
    	if parts.len == 2 and parts[0] != "" and parts[1] != "" then
      	alias = parts[0]
      	sym = parts[1]
      	currentNs = self.globalEnv.locals["__current_ns__"]
      	aliases = self.globalEnv.locals["__ns_aliases__"][currentNs]
      	if aliases.hasIndex(alias) then
          fullNs = aliases[alias]
      	else
        	fullNs = alias
      	end if
      	namespaces = self.globalEnv.locals["__namespaces__"]
      	if not namespaces.hasIndex(fullNs) then return self.lispError("No such namespace: " + fullNs)
      	if not namespaces[fullNs].hasIndex(sym) then return self.lispError("No such var: " + exp)
      	return @namespaces[fullNs][sym]
      end if
    end if

    	return env.get(@exp)  // walks the chain, errors if not found
    else
        return exp
    end if
end function

clojette.nativeFns = {
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
    "include_lib": @include_lib,
    "yield": @yield,
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
    "globals": @globals,
    "format-columns": @format_columns,
    "md5": @md5,
    "get-custom-object": @get_custom_object,
    "cob": @get_custom_object,
    "hash": @hash,
}

for kv in clojette.nativeFns
    clojette.globalEnv.locals[kv.key] = @kv.value
    clojette.globalEnv.natives[kv.key] = true
end for

clojette.tests = false

// REPL
clojette.repl = function(prompt="Clojette> ")
  while true
      input = user_input(prompt)
      if input == "exit" or input == "quit" or input == "q" then break
      input = "(do " + input + ")"
      result = self.eval(self.parse(input), self.globalEnv)
      if self.isError(@result) then
        print("ERROR: " + result["message"])
        if result.hasIndex("trace") and result["trace"].len > 0 then
          for frame in result["trace"]
            print(frame)
          end for
        end if
      else
        print(result)
      end if
  end while
end function

// Expose a function that evaluates code that is given to it :p
clojette.eval_clojette = function(code, env=self.globalEnv)
  return self.eval(self.parse(code), env)
end function
