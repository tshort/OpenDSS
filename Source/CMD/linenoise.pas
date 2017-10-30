{
 ----------------------------------------------------------
  Copyright (c) 2017 Battelle Memorial Institute
 ----------------------------------------------------------
}
unit linenoise;

{
/* linenoise.h -- guerrilla line editing library against the idea that a
 * line editing lib needs to be 20,000 lines of C code.
 *
 * See linenoise.c for more information.
 *
 * Copyright (c) 2010, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
}

{$mode delphi}

{$MACRO ON}
{$IFDEF Windows}
{$DEFINE LINENOISE_CALL:=stdcall;external 'liblinenoise'}
{$ELSE} // Darwin and Unix
{$linklib linenoise}
{$DEFINE LINENOISE_CALL:=cdecl;external}
{$ENDIF}

interface
//type
//  PlinenoiseCompletionCallback  = ^linenoiseCompletionCallback;
//  PlinenoiseCompletions  = ^linenoiseCompletions;

const
  LINENOISE_VERSION = '1.0.0';
  LINENOISE_VERSION_MAJOR = 1;
  LINENOISE_VERSION_MINOR = 1;

//procedure linenoiseSetCompletionCallback(fn:PlinenoiseCompletionCallback);LINENOISE_CALL;
//procedure linenoiseAddCompletion(lc:PlinenoiseCompletions; str:Pchar);LINENOISE_CALL;
function linenoise(prompt:Pchar):Pchar;LINENOISE_CALL;
procedure linenoiseFree(ptr:pointer);LINENOISE_CALL;
procedure linenoisePreloadBuffer(preloadText:Pchar);LINENOISE_CALL;
function linenoiseHistoryAdd(line:Pchar):longint;LINENOISE_CALL;
function linenoiseHistorySetMaxLen(len:longint):longint;LINENOISE_CALL;
function linenoiseHistoryLine(index:longint):Pchar;LINENOISE_CALL;
function linenoiseHistorySave(filename:Pchar):longint;LINENOISE_CALL;
function linenoiseHistoryLoad(filename:Pchar):longint;LINENOISE_CALL;
procedure linenoiseHistoryFree;LINENOISE_CALL;
procedure linenoiseClearScreen;LINENOISE_CALL;
procedure linenoiseSetMultiLine(ml:longint);LINENOISE_CALL;
procedure linenoisePrintKeyCodes;LINENOISE_CALL;
function linenoiseInstallWindowChangeHandler:longint;LINENOISE_CALL;
{ returns type of key pressed: 1 = CTRL-C, 2 = CTRL-D, 0 = other  }
function linenoiseKeyType:longint;LINENOISE_CALL;

implementation

end.
