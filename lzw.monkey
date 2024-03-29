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

Class ByteArray
    Field capacity:Int = 10
    Field arr:Int[]
    Field arrLength:Int = 0
    Field byteLength:Int = 0
    
    Method New()
        arr = New Int[capacity]
    End
    
    Method New(copy:ByteArray)
        Self.capacity = copy.capacity
        Self.arr = New Int[capacity]
        'This is actually faster than using the array slice copy
        'presumably because that uses reflection
        For Local i:Int = 0 Until copy.arrLength
            arr[i] = copy.arr[i]
        End
        arrLength = copy.arrLength
        byteLength = copy.byteLength
        If byteLength Mod 4 > 0
            arr[arrLength] = copy.arr[arrLength]
        End
    End
    
    Method New(s:String)
        capacity = s.Length/2+1
        arr = New Int[capacity]
        
        For Local i:Int = 0 Until s.Length
            Append(((s[i]&$0000FF00) Shr 8)&$FF)
            Append(s[i]&$FF)
        End
    End
    
    Method New(val:Int)
        arr = New Int[capacity]
        Append(val)    
    End
    
    Method Length:Int() Property
        Return byteLength    
    End
    
    Method Append:Void( arr:ByteArray )
        For Local i:Int = 0 Until arr.Length
            Append(arr.GetByte(i))
        End     
    End
    
    Method Append:Void(val:Int)
        If arrLength = capacity
            capacity *= 2
            arr = arr.Resize(capacity)
        End
        Local shift:Int = (3 - (byteLength Mod 4)) * 8
        arr[arrLength] = arr[arrLength]|((val&$FF) Shl shift)
        byteLength += 1
        If byteLength Mod 4 = 0
            arrLength += 1
        End
    End   
    
    Method Add:ByteArray(val:Int)
        Local copy:ByteArray = New ByteArray(Self)
        copy.Append(val)
        Return copy
    End
    
    Method CompareTo:Int( other:ByteArray )
        If other.byteLength <> Self.byteLength
            Return Self.byteLength - other.byteLength
        End
        
        Local len:Int = arrLength
        If byteLength Mod 4 > 0
            len += 1
        End
        Local ind:Int = 0
        
        While ind < byteLength
            Local si:Int = Self.GetByte(ind)
            Local oi:Int = other.GetByte(ind)
            If oi <> si
                Return si - oi
            End
            ind += 1
        End
        
        Return 0
    End
    
    Method PrintInts:Void()
        Local s:String
        For Local i:Int = 0 Until Length Step 2
            s += String((GetByte(i) Shl 8)|GetByte(i+1)) + ","
        End
        
        Print s
    End
    
    Method PrintBytes:Void()
        Local s:String = "["
        For Local i:Int = 0 Until Length
            s += GetByte(i) + ","
        End
        s += "]"
        Print s
    End
    
    Method GetByte:Int( index:Int )
        Local shift:Int = (3 - index Mod 4) Shl 3
        Local ret:Int = ((arr[index Shr 2] & ($FF Shl shift)) Shr shift)&$FF
        Return ret
    End
    
    Method ToYEnc:String()
        Local out:String[] = New String[byteLength*2] 'Overkill
        Local sIndex:Int = 0
        
        For Local i:Int = 0 Until byteLength
            Local bVal:Int = GetByte(i)
            Local outVal = (bVal + 42) Mod 256
            
            If outVal = $00 Or outVal = $0A Or outVal = $0D Or outVal = $3D
                out[sIndex] = "="
                sIndex += 1
                outVal = (bVal + 64) Mod 256
            End
            
            out[sIndex] = String.FromChar(outVal) 
            sIndex += 1
                
        End
        
        Return "".Join(out)
    End
    
    Function FromYEnc:ByteArray(in:String)
        Local ba:ByteArray = New ByteArray()
        
        For Local i:Int = 0 Until in.Length
            Local bVal:Int = in[i]
            
            If bVal = $3D
                i += 1
                bVal = in[i]
                bVal -= 64
            Else
                bVal -= 42
            End
            
            If bVal < 0
                bVal += 256
            End
            ba.Append(bVal)
        End
        
        Return ba
    End
    
    Method ToString:String()
        Local strArr:String[]
        If byteLength Mod 4 > 0 
            If byteLength Mod 4 > 2 
                strArr = New String[arrLength*2+2]
            Else  
                strArr = New String[arrLength*2+1]
            End
        Else
            strArr = New String[arrLength*2]
        End
        For Local i:Int = 0 Until arrLength
            strArr[i*2] = String.FromChar((arr[i]&$FFFF0000) Shr 16)
            strArr[i*2+1] = String.FromChar(arr[i]&$0000FFFF)            
        End
        Local remBytes:Int = byteLength - arrLength*4
        If remBytes > 0
            strArr[arrLength*2] = String.FromChar(((arr[arrLength]&$FFFF0000) Shr 16) & $FFFF)
            If remBytes > 2
                strArr[arrLength*2+1] = String.FromChar(arr[arrLength]&$0000FFFF)
            End
        End
        Return "".Join(strArr)
    End
    
    Method ObjectEnumerator:ByteArrayObjectEnumerator()
        Return New ByteArrayObjectEnumerator(arr,byteLength)
    End
End

Class ByteArrayObjectEnumerator
	Field arr:Int[]
    Field byteLength:Int
    Field currByte:Int = 0
    
    Method New( arr:Int[], byteLength:Int)
		Self.arr = arr
        Self.byteLength = byteLength
	End

	Method HasNext()
		Return currByte < byteLength
	End
	
	Method NextObject:IntObject()
        Local ind:Int = currByte/4
        Local shift:Int = (3 - currByte Mod 4) * 8
        currByte += 1
		Return New IntObject(((arr[ind] & $FF) Shl shift) Shr shift)
	End
End

Class ByteArrayMap<V> Extends Map<ByteArray,V>

	Method Compare( lhs:ByteArray,rhs:ByteArray )
		Return lhs.CompareTo(rhs)
	End

End

#rem
' summary:This class provides string compression and decompression functions based on the LZW
' algorithm. It is based on implementations found at http://rosettacode.org/wiki/LZW_compression
#end
Class LZW
    
    Private
    Global markerWord1:Int = "L"[0] Shl 8 + "Z"[0]
    Global markerWord2:Int = "W"[0] Shl 8 + "S"[0]
    Global markerString:String = String.FromChar(markerWord1) + String.FromChar(markerWord2)     
    Global compressDict:ByteArrayMap<IntObject> = New ByteArrayMap<IntObject>()
    Global uncompressDict:ByteArray[] = []
    
    Public
    
    Function IsLZWString:Bool( in:String )
        If in[0] = markerWord1 And in[1] = markerWord2
            Return True
        End
        Return False
    End
    
#rem
    summary: Takes an input string and returns the LZW compressed version
#end
    Function CompressString:String(input:String)                
        
        Local w:ByteArray = New ByteArray()
        Local ia:ByteArray = New ByteArray(input)
        Local result:String[] = New String[ia.Length+1]
        
        Local compressDict:ByteArrayMap<IntObject> = ResetEncodeDict()
        Local dictSize:Int = baseDictSize
        
        For Local i:Int = 0 Until ia.Length
            Local byte:Int = ia.GetByte(i)
            Local wc:ByteArray = w.Add(byte)
                   
            If (compressDict.Contains(wc))
                w = wc
            Else
                Local code:Int = compressDict.Get(w)
                result[i] = String.FromChar(code)
                ' Add wc to the dictionary.
                compressDict.Set(wc, dictSize)
                dictSize += 1
                If dictSize = maxDictSize
                    compressDict = ResetEncodeDict()
                    dictSize = baseDictSize
                End
                w = New ByteArray(byte)
            End
        End
        ' Output the code for w.
        If w.Length() > 0
            result[ia.Length] = String.FromChar(compressDict.Get(w))
        End
        Return markerString + "".Join(result)
        'Local ba:ByteArray = New ByteArray("".Join(result))
        'Return ba.ToYEnc()
        
    End
    
    Private
    
    Global maxDictSize:Int = $3FFF
    Global baseDictSize:Int
    
    Function ResetEncodeDict:ByteArrayMap<IntObject>()
        baseDictSize = 256 + avoidZero
        compressDict.Clear()
        
        For Local i:Int = avoidZero Until baseDictSize
            Local ba:ByteArray = New ByteArray()
            ba.Append(i-avoidZero)
            compressDict.Set(ba, i)
        End
        'Add 0 combinations
        For Local i:Int = 0 Until baseDictSize
            Local ba:ByteArray = New ByteArray()
            ba.Append(i)
            ba.Append(0)
            compressDict.Set(ba, baseDictSize+i)
            ba = New ByteArray()
            ba.Append(0)
            ba.Append(i)
            compressDict.Set(ba, baseDictSize+256+i)
        End
        
        baseDictSize += 512
        Return compressDict
    End
    
    Function ResetDecodeDict:ByteArray[]()
        baseDictSize = 256 + avoidZero
                
        'Trading memory for performance here by using an array rather than a map
        If uncompressDict.Length = 0
            uncompressDict = New ByteArray[maxDictSize]
            For Local i:Int = avoidZero Until baseDictSize
                uncompressDict[i] = New ByteArray(i-avoidZero)
            End 
            'Add 0 combinations
            For Local i:Int = 0 Until baseDictSize
                Local ba:ByteArray = New ByteArray()
                ba.Append(i)
                ba.Append(0)
                uncompressDict[baseDictSize+i] = ba
                
                ba = New ByteArray()
                ba.Append(0)
                ba.Append(i)
                uncompressDict[baseDictSize+256+i] = ba
            End
        
        End
        
        baseDictSize += 512
        Return uncompressDict[..]
    End
    
    Public
#rem
    summary: Takes a string compressed with CompressString and returns the uncompressed version.
    Set discardDict to True to recover the memory used for the decompression dictionary
#end
    Const avoidZero:Int = 1
    Function DecompressString:String(compressed:String, discardDict:Bool = False, force:Bool = False)
    
        Local i:Int = 0
        
        If Not IsLZWString(compressed)
            If Not force
                Print "Input string is not marked as LZW compressed, returning without decompressing."
                Return compressed
            End
        Else
            i = 2    
        End
        
        Local dictionary:ByteArray[] = ResetDecodeDict()
        Local dictSize:Int = baseDictSize
        Local w:ByteArray = New ByteArray()
        'Local sa:ByteArray = ByteArray.FromYEnc(compressed)
        'compressed = sa.ToString()
        'sa = New ByteArray()
        Local sa:ByteArray = New ByteArray()
        
        While i < compressed.Length
            Local k:Int = compressed[i]
            Local entry:ByteArray
            
            If dictionary[k] And dictionary[k].Length > 0
                entry = dictionary[k]
            ElseIf k = dictSize
                entry = w.Add(w.GetByte(0))
            Else
                Print "LZW - unknown dictionary key: " + k
                Return ""
            End
            sa.Append( entry )
            
            If w.Length > 0
                'Add w+entry[0] to the dictionary.
                dictionary[dictSize] = w.Add(entry.GetByte(0))
                dictSize += 1
                If dictSize = maxDictSize
                    'Print "resetting dict"
                    dictionary = ResetDecodeDict()
                    dictSize = baseDictSize
                End
            End
            w = entry
            i += 1
        End
        
        If discardDict
            uncompressDict = []
        End
        Return sa.ToString()
    End
 End
 