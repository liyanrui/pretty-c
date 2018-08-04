-- Copyright 2010 Renaud Aubin <renaud.aubin@gmail.com>
-- Time-stamp: <2010-12-06 20:59:28>
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- This work is fully inspired by Peter Münster's pret-c module.
--
if not modules then modules = { } end modules ['t-pretty-c'] = {
    version   = 1.501,
    comment   = "Companion to t-pretty-c.mkiv",
    author    = "Renaud Aubin",
    copyright = "2010 Renaud Aubin",
    license   = "GNU General Public License version 3"
}

local tohash = table.tohash
local P, S, V, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.patterns


local keyword = tohash {
   "auto", "break", "case", "const", "continue", "default", "do",
   "else", "enum", "extern", "for", "goto", "if", "register", "return",
   "sizeof", "static", "struct", "switch", "typedef", "union", "volatile",
   "while",
}

local type = tohash {
   "char", "double", "float", "int", "long", "short", "signed", "unsigned",
   "void",
}

local preproc = tohash {
   "define", "include", "pragma", "if", "ifdef", "ifndef", "elif", "endif",
   "defined",
}

local context               = context
local verbatim              = context.verbatim
local makepattern           = visualizers.makepattern

local CSnippet              = context.CSnippet
local startCSnippet         = context.startCSnippet
local stopCSnippet          = context.stopCSnippet

local CSnippetBoundary      = verbatim.CSnippetBoundary
local CSnippetSpecial       = verbatim.CSnippetSpecial
local CSnippetComment       = verbatim.CSnippetComment
local CSnippetKeyword       = verbatim.CSnippetKeyword
local CSnippetType          = verbatim.CSnippetType
local CSnippetPreproc       = verbatim.CSnippetPreproc
local CSnippetName          = verbatim.CSnippetName
local CSnippetString        = verbatim.CSnippetString

local typedecl = false

local function visualizename_a(s)
   if keyword[s] then
      CSnippetKeyword(s)
      typedecl=false
   elseif type[s] then
      CSnippetType(s)
      typedecl=true
   elseif preproc[s] then
      CSnippetPreproc(s)
      typedecl=false
   else 
      verbatim(s)
      typedecl=false
   end
end

local function visualizename_b(s)
   if(typedecl) then
      CSnippetName(s)
      typedecl=false
   else
      visualizename_a(s)
   end
end

local function visualizename_c(s)
   if(typedecl) then
      CSnippetBoundary(s)
      typedecl=false
   else
      visualizename_a(s)
   end
end

local handler = visualizers.newhandler {
    startinline  = function() CSnippet(false,"{") end,
    stopinline   = function() context("}") end,
    startdisplay = function() startCSnippet() end,
    stopdisplay  = function() stopCSnippet() end ,

    boundary     = function(s) CSnippetBoundary(s) end,
    comment      = function(s) CSnippetComment(s) end,
    string       = function(s) CSnippetString(s) end,
    name         = function(s) CSnippetName(s) end,
    type         = function(s) CSnippetType(s) end,
    preproc      = function(s) CSnippetPreproc(s) end,
    varname      = function(s) CSnippetVarName(s) end,

    name_a       = visualizename_a,
    name_b       = visualizename_b,
    name_c       = visualizename_c,
}

local space       = patterns.space
local anything    = patterns.anything
local newline     = patterns.newline
local emptyline   = patterns.emptyline
local beginline   = patterns.beginline
local somecontent = patterns.somecontent

local comment     = P("//") * patterns.space^0 * (1 - patterns.newline)^0
local incomment_open = P("/*")
local incomment_close = P("*/")

local name        = (patterns.letter + patterns.underscore)
                  * (patterns.letter + patterns.underscore + patterns.digit)^0
local boundary    = S('{}')

local grammar = visualizers.newgrammar(
   "default",
   {
      "visualizer",

      ltgtstring = makepattern(handler,"string",P("<")) * V("space")^0
      * (makepattern(handler,"string",patterns.utf8character-patterns.newline-P(">")))^0
   * makepattern(handler,"string",P(">")+patterns.newline),


      sstring = makepattern(handler,"string",patterns.dquote)
      * ( V("whitespace") + makepattern(handler,"string",(P("\\")*P(1))+patterns.utf8character-patterns.dquote) )^0
      * makepattern(handler,"string",patterns.dquote),

      dstring = makepattern(handler,"string",patterns.squote)
      * ( V("whitespace") + makepattern(handler,"string",(P("\\")*P(1))+patterns.utf8character-patterns.squote) )^0
      * makepattern(handler,"string",patterns.squote),

      comment = makepattern(handler,"comment",comment),
      --       * (V("space") + V("content"))^0,

      incomment = makepattern(handler,"comment",incomment_open)
      * ( V("whitespace") + makepattern(handler,"comment", patterns.utf8character-incomment_close) )^0
      * makepattern(handler,"comment",incomment_close),
   
      argsep = V("optionalwhitespace") * makepattern(handler,"default",P(",")) * V("optionalwhitespace"),
      argumentslist = V("optionalwhitespace") * (makepattern(handler,"name",name) + V("argsep"))^0,

      preproc = makepattern(handler,"preproc", P("#")) * V("optionalwhitespace") * makepattern(handler,"preproc", name) * V("whitespace") 
      * (
         (makepattern(handler,"boundary", name) * makepattern(handler,"default",P("(")) * V("argumentslist") * makepattern(handler,"default",P(")")))
         + ((makepattern(handler,"name", name) * (V("space")-V("newline"))^1 ))
        )^-1,

      name = (makepattern(handler,"name_c", name) * V("optionalwhitespace") * makepattern(handler,"default",P("(")))
      + (makepattern(handler,"name_b", name) * V("optionalwhitespace") * makepattern(handler,"default",P("=") + P(";") + P(")") + P(",") ))
      + makepattern(handler,"name_a",name),

    pattern =
      V("incomment")
      + V("comment")
      + V("ltgtstring")
      + V("dstring")
      + V("sstring")
      + V("preproc")
      + V("name")
      + makepattern(handler,"boundary",boundary)
      + V("space")
      + V("line")
      + V("default"),

    visualizer =
        V("pattern")^1
   }
)

local parser = P(grammar)

visualizers.register("c", { parser = parser, handler = handler, grammar = grammar } )
