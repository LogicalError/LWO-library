#region LICENSE
// Copyright (c) 2011 Sander van Rossen
//  
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//  
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//  
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
#endregion

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace Lightwave
{
	public class ChunkReader : IDisposable
	{
		public ChunkReader(byte[] buffer) { buffer_size = buffer.Length; reader = new BinaryReader(new MemoryStream(buffer)); }
		public void Dispose() { reader.Dispose(); }
		readonly BinaryReader reader;
		
		readonly long buffer_size;

		public int BytesLeft
		{
			get
			{
				return (int)Math.Min(int.MaxValue, (long)(buffer_size - reader.BaseStream.Position));
			}
		}

		public byte[] ReadBytes(uint size) { return reader.ReadBytes((int)size); }
		public ChunkReader GetSubChunk(uint size) { return new ChunkReader(reader.ReadBytes((int)size)); }

		public float[] ReadSingleArray(int num)
		{
			var values = new float[num];
			for (var i = 0; i < num; i++)
				values[i] = ReadSingle();
			return values;
		}

		public sbyte			ReadSByte() { return reader.ReadSByte(); }
		public short			ReadSInt16() { return Swap(reader.ReadInt16()); }
		public int				ReadSInt32() { return Swap(reader.ReadInt32()); }
		public byte				ReadUInt8() { return reader.ReadByte(); }
		public ushort			ReadUInt16() { return Swap(reader.ReadUInt16()); }
		public uint				ReadUInt32() { return Swap(reader.ReadUInt32()); }
		public float			ReadSingle() { return Swap(reader.ReadSingle()); }
		public uint				ReadVariableLengthIndex() 
		{
			uint vx = 0;
			var ch = reader.ReadByte();
			if (ch == 0xff)
			{
				ch = reader.ReadByte(); vx |= (uint)(((uint)ch) << 16);
				ch = reader.ReadByte(); vx |= (uint)(((uint)ch) << 8);
				ch = reader.ReadByte(); vx |= ch;
			} else
			{
				vx |= (uint)(((uint)ch) << 8);
				ch = reader.ReadByte(); vx |= ch;
			}
			return vx;
		}
		public T ReadID<T>() where T : struct, IComparable, IFormattable, IConvertible //enum
		{
			if (!typeof(T).IsEnum) throw new ArgumentException("T must be an enumerated type");

			var value = Swap(reader.ReadUInt32());
			foreach (T item in Enum.GetValues(typeof(T)))
			{
				if (Convert.ToUInt32(item) == value)
				{
					return item;
				}
			}
			return (T)Enum.ToObject(typeof(T), value);// default(T);
		}

		public string ReadString(uint size)
		{
			var bytes = ReadBytes(size);
			return ASCIIEncoder.GetString(bytes);
		}

		public string ReadString()
		{
			var bytes = new List<byte>();
			byte ch;
			int bytesread = 0;

			do
			{
				ch = reader.ReadByte();
				bytesread ++;
				if (ch != 0)
					bytes.Add(ch);
			} while (ch > 0);

			// If the length of the string is odd, skip the extra padding byte
			if ((bytesread & 1) == 1)
				ch = reader.ReadByte();

			return ASCIIEncoder.GetString(bytes.ToArray());
		}

		readonly ASCIIEncoding ASCIIEncoder = new ASCIIEncoding();

		// Swap methods to change little endianness (BinaryReader is little endian) to big endianness
		byte	Swap(byte v)	{ return v; }
		sbyte	Swap(sbyte v)	{ return v; }
		short	Swap(short v)	{ return (short)(((v & 0xff) << 8) | ((v >> 8) & 0xff)); }
		ushort	Swap(ushort v)	{ return (ushort)(((v & 0xff) << 8) | ((v >> 8) & 0xff)); }
		int		Swap(int v)		{ return (int)(((Swap((short)v) & 0xffff) << 0x10) | (Swap((short)(v >> 0x10)) & 0xffff)); }
		uint	Swap(uint v)	{ return (uint)(((Swap((ushort)v) & 0xffff) << 0x10) | (Swap((ushort)(v >> 0x10)) & 0xffff)); }
		long	Swap(long v)	{ return (long)(((Swap((int)v) & 0xffffffffL) << 0x20) | (Swap((int)(v >> 0x20)) & 0xffffffffL)); }
		ulong	Swap(ulong v)	{ return (ulong)(((Swap((uint)v) & 0xffffffffL) << 0x20) | (Swap((uint)(v >> 0x20)) & 0xffffffffL)); }
		float	Swap(float v)	{ byte[] temp = BitConverter.GetBytes(v); Array.Reverse(temp); return BitConverter.ToSingle(temp, 0); }

		public string GetIDString(uint value)
		{
			byte[] temp = BitConverter.GetBytes(value);
			return ASCIIEncoder.GetString(temp.Reverse().ToArray(), 0, 4);
		}
	}
}
