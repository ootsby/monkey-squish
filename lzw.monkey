#rem
'/*
'* Copyright (c) 2011, Damian Sinclair
'*
'* All rights reserved.
'* Redistribution and use in source and binary forms, with or without
'* modification, are permitted provided that the following conditions are met:
'*
'*   - Redistributions of source code must retain the above copyright
'*     notice, this list of conditions and the following disclaimer.
'*   - Redistributions in binary form must reproduce the above copyright
'*     notice, this list of conditions and the following disclaimer in the
'*     documentation and/or other materials provided with the distribution.
'*
'* THIS SOFTWARE IS PROVIDED BY THE SQUISH PROJECT CONTRIBUTORS "AS IS" AND
'* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
'* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
'* DISCLAIMED. IN NO EVENT SHALL THE SQUISH PROJECT CONTRIBUTORS BE LIABLE
'* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
'* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
'* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
'* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
'* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
'* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
'* DAMAGE.
'*/
#end

#rem
' summary:This class provides string compression and decompression functions based on the LZW
' algorithm. It is based on implementations found at http://rosettacode.org/wiki/LZW_compression
#end
Class LZW
    
    Private
    
    Global compressDict:StringMap<IntObject> = New StringMap<IntObject>()
    Global uncompressDict:String[] = []
    
    Public
    
#rem
    summary: Takes an input string and returns the LZW compressed version
#end
    Function CompressString:String(input:String)
        Local dictSize:Int = 256
        compressDict.Clear()
        
        For Local i:Int = 0 Until dictSize
            compressDict.Set(String.FromChar(i), i)
        End
        
        Local w:String = ""
        
        'storing sections in an array and then using join is slightly slower on some targets
        'but hugely faster on android
        Local sa:String[] = New String[input.Length+1]
        
        For Local i:Int = 0 Until input.Length() 
            Local cs:String = String.FromChar(input[i])
            Local wc:String = w + cs
            If (compressDict.Contains(wc))
                w = wc
            Else
                sa[i] = String.FromChar(compressDict.Get(w))
                ' Add wc to the dictionary.
                compressDict.Set(wc, dictSize)
                dictSize += 1
                w = cs
            End
        End
        ' Output the code for w.
        If w.Length() > 0
            sa[input.Length] = String.FromChar(compressDict.Get(w))
        End
        
        Return "".Join(sa)
        
    End
    
#rem
    summary: Takes a string compressed with CompressString and returns the uncompressed version.
    Set discardDict to True to recover the memory used for the decompression dictionary
#end
    Function DecompressString:String(compressed:String, discardDict:Bool = False)
        Local dictSize:Int = 256
        
        'Trading memory for performance here by using an array rather than a map
        If uncompressDict.Length = 0
            uncompressDict = New String[65536]
            For Local i:Int = 0 Until dictSize
                uncompressDict[i] = String.FromChar(i)
            End 
        End
        
        Local dictionary:String[] = uncompressDict[..]
        
        Local w:String = "" + String.FromChar(compressed[0])
        Local sa:String[] = New String[compressed.Length]
        sa[0] = w
        
        For Local i:Int = 1 Until compressed.Length
            Local k:Int = compressed[i]
            Local entry:String
            If dictionary[k]
                entry = dictionary[k]
            ElseIf k = dictSize
                entry = w + String.FromChar(w[0])
            Else
                Error "LZW - unknown dictionary key: " + k
            End
            sa[i] = entry
            
            'Add w+entry[0] to the dictionary.
            dictionary[dictSize] = w + String.FromChar(entry[0])
            dictSize += 1
 
            w = entry
        End
        If discardDict
            uncompressDict = []
        End
        Return "".Join(sa)
    End
 End
 