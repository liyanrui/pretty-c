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
local P, S, V, patterns, C = lpeg.P, lpeg.S, lpeg.V, lpeg.patterns, lpeg.C


local keyword = tohash {
   "auto", "break", "case", "const", "continue", "default", "do",
   "else", "enum", "extern", "for", "goto", "if", "register", "return",
   "sizeof", "static", "struct", "switch", "typedef", "union", "volatile",
   "while",
}

local type = tohash {
   "char", "double", "float", "int", "long", "short", "signed", "unsigned",
   "void", "size_t",
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
local CSnippetComment       = verbatim.CSnippetComment
local CSnippetKeyword       = verbatim.CSnippetKeyword
local CSnippetType          = verbatim.CSnippetType
local CSnippetPreproc       = verbatim.CSnippetPreproc
local CSnippetName          = verbatim.CSnippetName
local CSnippetString        = verbatim.CSnippetString
local CSnippetProcName      = verbatim.CSnippetProcName

local typedecl = false
local letter = patterns.letter
local underscore = patterns.underscore
local digit = patterns.digit
local space = patterns.space
local name = C((letter + underscore) * (letter + underscore + digit)^0)
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


local langle = "\\color[darkblue]{\\switchtobodyfont[rm]\\raise.1em\\hbox{$\\langle$}\\,"
local rangle_equiv = "\\,\\raise.1em\\hbox{$\\rangle\\equiv$}}"
local rangle = "\\,\\raise.1em\\hbox{$\\rangle$}}"
local handler = visualizers.newhandler {
    startinline  = function() CSnippet(false,"{") end,
    stopinline   = function() context("}") end,
    startdisplay = function() startCSnippet() end,
    stopdisplay  = function() stopCSnippet() end ,

    boundary     = function(s) CSnippetBoundary(s) end,
    comment      = function(s) CSnippetComment(s)  end,
    string       = function(s) CSnippetString(s) end,
    name         = function(s) CSnippetName(s) end,
    type         = function(s) CSnippetType(s) end,
    preproc      = function(s) CSnippetPreproc(s) end,
    varname      = function(s) CSnippetVarName(s) end,

    -- local str = P("@") * P(" ")^0 * lpeg.C((1 - P(" ")^0 * P("#"))^0) * P("#") * P(-1)
    -- str:match("@ test #")
    procname     = function(s) context(langle .. string.gsub(s, "^@%s*(.-)%s*#$", "%1") .. rangle_equiv) end,
    procnameref  = function(s) context(langle .. string.gsub(s, "^#%s*(.-)%s*@$", "%1") .. rangle) end,

    name_a       = visualizename_a,
    name_b       = visualizename_b,
    name_c       = visualizename_c,
}

local newline     = patterns.newline
local dquote      = patterns.dquote
local squote      = patterns.squote

local comment     = P("//") * patterns.space^0 * (1 - newline)^0
local incomment_open = P("/*")
local incomment_close = P("*/")

local boundary    = S('{}')

local grammar = visualizers.newgrammar(
   "default",
   {
      "visualizer",

      ltgtstring = makepattern(handler,"string",P("<") * (1 - newline - P(">"))^0 * P(">")),
      dstring = makepattern(handler, "string", dquote * ((P("\\")*P(1)) + 1 - dquote)^0 * dquote),
      sstring = makepattern(handler, "string", squote * ((P("\\")*P(1)) + 1 - squote)^0 * squote),

      comment = makepattern(handler,"comment",comment),
      incomment = makepattern(handler, "comment", incomment_open * (1-incomment_close)^0 * incomment_close),
   
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

      procname = makepattern(handler, "procname", P("@") * (P("\\\\") * space^0 * newline + 1 - P("#") - newline)^0 * P("#")),
      procnameref = makepattern(handler, "procnameref", P("#") * (P("\\\\") * space^0 * newline + 1 - P("@") - newline)^0 * P("@")),
      
      pattern =
          V("procname")
          + V("procnameref")
          + V("incomment")
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
      
      visualizer = V("pattern")^1
   }
)

local parser = P(grammar)
visualizers.register("c", { parser = parser, handler = handler, grammar = grammar } )
