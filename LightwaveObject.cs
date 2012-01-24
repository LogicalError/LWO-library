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
	#region 'plug-in'

	public enum PluginType : uint
	{
		ID_FUNC = (uint)(((uint)'F') << 24 | ((uint)'U') << 16 | ((uint)'N') << 8 | ((uint)'C')),
		ID_ENAB = (uint)(((uint)'E') << 24 | ((uint)'N') << 16 | ((uint)'A') << 8 | ((uint)'B')),
	}

	// plug-in reference 
	public class LightwavePlugin 
	{
		public string			ord;
		public string			name;
		public int				flags;
		public byte[]			data;
		
		public static LightwavePlugin ReadShader(ChunkReader reader)
		{
			var shader = new LightwavePlugin();

			var hsz		= reader.ReadUInt16(); // Q: example code does this, but can't find this in documentation?
			shader.ord	= reader.ReadString();
			
			using (var headerReader = reader.GetSubChunk(hsz))
			{
				while (headerReader.BytesLeft > 0)
				{
					var id = headerReader.ReadID<PluginType>();
					var sz = headerReader.ReadUInt16();
					sz += (ushort)(sz & 1);
					hsz -= sz;
					if (id == PluginType.ID_ENAB)
					{
						shader.flags = headerReader.ReadUInt16();
						break;
					}
				}
			}
			
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<PluginType>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);
				
				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case PluginType.ID_FUNC:
						{
							shader.name = subChunkReader.ReadString();
							shader.data = subChunkReader.ReadBytes((uint)subChunkReader.BytesLeft);
							break;
						}

						default:
							//Console.WriteLine("Unknown shader type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return shader;
		}
	};
	#endregion

	#region Envelope

	// values that can be enveloped 
	public class EnvelopeParameter
	{
		public float	value;
		public uint		envelope_index;
	};

	public class VectorParameter
	{
		public readonly float[] values = new float[3];
		public uint		envelope_index;
	};

	public enum KeyShape : uint
	{
		ID_TCB		= (uint)(((uint)'T') << 24 | ((uint)'C') << 16 | ((uint)'B') << 8 | ((uint)' ')),
		ID_HERM		= (uint)(((uint)'H') << 24 | ((uint)'E') << 16 | ((uint)'R') << 8 | ((uint)'M')),
		ID_BEZI		= (uint)(((uint)'B') << 24 | ((uint)'E') << 16 | ((uint)'Z') << 8 | ((uint)'I')),
		ID_BEZ2		= (uint)(((uint)'B') << 24 | ((uint)'E') << 16 | ((uint)'Z') << 8 | ((uint)'2')),
		ID_LINE		= (uint)(((uint)'L') << 24 | ((uint)'I') << 16 | ((uint)'N') << 8 | ((uint)'E')),
		ID_STEP		= (uint)(((uint)'S') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'P'))
	}

	// envelopes 
	public class EnvelopeKey 
	{
		public EnvelopeKey(float time, float value) { Time = time; Value = value; }
		public float			Value;
		public float			Time;
		public KeyShape			Shape;               // ID_TCB, ID_BEZ2, etc. 
		public float			Tension;
		public float			Continuity;
		public float			Bias;
		public readonly float[]	param = new float[4];
	};

	public enum EvalType : ushort
	{
		BEH_RESET		= 0,
		BEH_CONSTANT	= 1,
		BEH_REPEAT		= 2,
		BEH_OSCILLATE	= 3,
		BEH_OFFSET		= 4,
		BEH_LINEAR		= 5
	};

	public enum EnvelopeType : uint
	{
		ID_PRE  = (uint)(((uint)'P') << 24 | ((uint)'R') << 16 | ((uint)'E') << 8 | ((uint)' ')),
		ID_POST = (uint)(((uint)'P') << 24 | ((uint)'O') << 16 | ((uint)'S') << 8 | ((uint)'T')),
		ID_KEY  = (uint)(((uint)'K') << 24 | ((uint)'E') << 16 | ((uint)'Y') << 8 | ((uint)' ')),
		ID_SPAN = (uint)(((uint)'S') << 24 | ((uint)'P') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_CHAN = (uint)(((uint)'C') << 24 | ((uint)'H') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_TYPE = (uint)(((uint)'T') << 24 | ((uint)'Y') << 16 | ((uint)'P') << 8 | ((uint)'E')),
		ID_NAME = (uint)(((uint)'N') << 24 | ((uint)'A') << 16 | ((uint)'M') << 8 | ((uint)'E'))
	}

	public class Envelope 
	{
		public uint							Index;
		public int							Type;
		public string						Name;
		public readonly List<EnvelopeKey>	Keys = new List<EnvelopeKey>(); // linked list of keys 				
		
		public EvalType						PreBehavior;			// pre and post (extrapolation) 
		public EvalType						PostBehavior;			// pre and post (extrapolation) 
		public readonly List<LightwavePlugin>	ChannelFilters = new List<LightwavePlugin>(); // linked list of channel filters 
		
		public static Envelope ReadEnvelope(ChunkReader reader)
		{
			var env = new Envelope(); // allocate the Envelope structure 
			env.Index = reader.ReadVariableLengthIndex(); // index

			EnvelopeKey lastKey = null;
			while (reader.BytesLeft > 0)
			{
				// process subchunks as they're encountered 
				var id = reader.ReadID<EnvelopeType>();
				var sz = reader.ReadUInt16();		
				sz += (ushort)(sz & 1);

				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case EnvelopeType.ID_TYPE:
						{
							env.Type = subChunkReader.ReadUInt16();
							break;
						}

						case EnvelopeType.ID_NAME:
						{
							env.Name = subChunkReader.ReadString();
							break;
						}

						case EnvelopeType.ID_PRE:
						{
							env.PreBehavior = (EvalType)subChunkReader.ReadUInt16();
							break;
						}

						case EnvelopeType.ID_POST:
						{
							env.PostBehavior = (EvalType)subChunkReader.ReadUInt16();
							break;
						}

						case EnvelopeType.ID_KEY:
						{
							lastKey = new EnvelopeKey(
												subChunkReader.ReadSingle(),	// time
												subChunkReader.ReadSingle()		// value
												);
							env.Keys.Add(lastKey);

							//TODO: not sort all the time
							env.Keys.Sort((Comparison<EnvelopeKey>)delegate(EnvelopeKey k1, EnvelopeKey k2)
							{
								return k1.Time > k2.Time ? 1 : k1.Time < k2.Time ? -1 : 0;
							});
							break;
						}

						case EnvelopeType.ID_SPAN:
						{
							if (lastKey == null) // We should've encountered an ID_KEY before an ID_SPAN
								throw new Exception("Key not defined"); //TODO: make proper exception class

							lastKey.Shape = subChunkReader.ReadID<KeyShape>();
							switch (lastKey.Shape)
							{
								case KeyShape.ID_TCB:
								{
									lastKey.Tension		= subChunkReader.ReadSingle();
									lastKey.Continuity	= subChunkReader.ReadSingle();
									lastKey.Bias		= subChunkReader.ReadSingle();
									break;
								}

								case KeyShape.ID_BEZI:
								case KeyShape.ID_HERM:
								case KeyShape.ID_BEZ2:
								{
									Array.Clear(lastKey.param, 0, lastKey.param.Length);
									int i = 0;
									while (i < 4 && subChunkReader.BytesLeft > 0)
									{
										lastKey.param[i] = subChunkReader.ReadSingle();
										i++;
									}
									break;
								}

								case KeyShape.ID_LINE:
									break;

								default:
									Console.WriteLine("Unknown envelope span shape type " + reader.GetIDString((uint)lastKey.Shape));
									break;
							}
							break;
						}

						case EnvelopeType.ID_CHAN:
						{
							var plug = new LightwavePlugin();
							plug.name	= subChunkReader.ReadString();
							plug.flags	= subChunkReader.ReadUInt16();
							plug.data	= subChunkReader.ReadBytes((uint)reader.BytesLeft);
							env.ChannelFilters.Add(plug);
							break;
						}

						default:
							Console.WriteLine("Unknown envelope type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return env;
		}
		
		#region envelope evaluation
		//======================================================================
		// Range()
		//
		// Given the value v of a periodic function, returns the equivalent value
		// v2 in the principal interval [lo, hi].  If i isn't NULL, it receives
		// the number of wavelengths between v and v2.
		//
		//   v2 = v - i * (hi - lo)
		//
		// For example, range( 3 pi, 0, 2 pi, i ) returns pi, with i = 1.
		//====================================================================== 
		static float Range(float v, float lo, float hi, ref int i)
		{
			float v2, r = hi - lo;

			if (r == 0.0)
			{
				i = 0;
				return lo;
			}

			v2 = lo + v - r * (float)Math.Floor((double)v / r);
			i = -(int)((v2 - v) / r + (v2 > v ? 0.5 : -0.5));

			return v2;
		}

		//======================================================================
		//hermite()
		//
		//Calculate the Hermite coefficients.
		//====================================================================== 
		static void Hermite(float t, out float h1, out float h2, out float h3, out float h4)
		{
			var t2 = t * t;
			var t3 = t * t2;

			h2 = 3.0f * t2 - t3 - t3;
			h1 = 1.0f - h2;
			h4 = t3 - t2;
			h3 = h4 - t2 + t;
		}


		//======================================================================
		//bezier()
		//
		//Interpolate the value of a 1D Bezier curve.
		//====================================================================== 
		static float Bezier(float x0, float x1, float x2, float x3, float t)
		{
			float a, b, c, t2, t3;

			t2 = t * t;
			t3 = t2 * t;

			c = 3.0f * (x1 - x0);
			b = 3.0f * (x2 - x1) - c;
			a = x3 - x0 - c - b;

			return a * t3 + b * t2 + c * t + x0;
		}

		//======================================================================
		//bez2_time()
		//
		//Find the t for which bezier() returns the input time.  The handle
		//endpoints of a BEZ2 curve represent the control points, and these have
		//(time, value) coordinates, so time is used as both a coordinate and a
		//parameter for this curve type.
		//====================================================================== 
		static float Bez2_time(float x0, float x1, float x2, float x3, float time, ref float t0, ref float t1)
		{
			float v, t;

			t = t0 + (t1 - t0) * 0.5f;
			v = Bezier(x0, x1, x2, x3, t);
			if (Math.Abs(time - v) > .0001f)
			{
				if (v > time)
					t1 = t;
				else
					t0 = t;
				return Bez2_time(x0, x1, x2, x3, time, ref t0, ref t1);
			}
			else
				return t;
		}


		//======================================================================
		//bez2()
		//
		//Interpolate the value of a BEZ2 curve.
		//====================================================================== 
		static float Bez2(EnvelopeKey key0, EnvelopeKey key1, float time)
		{
			float x, y, t, t0 = 0.0f, t1 = 1.0f;

			if (key0.Shape == KeyShape.ID_BEZ2)
				x = key0.Time + key0.param[2];
			else
				x = key0.Time + (key1.Time - key0.Time) / 3.0f;

			t = Bez2_time(key0.Time, x, key1.Time + key1.param[0], key1.Time, time, ref t0, ref t1);

			if (key0.Shape == KeyShape.ID_BEZ2)
				y = key0.Value + key0.param[3];
			else
				y = key0.Value + key0.param[1] / 3.0f;

			return Bezier(key0.Value, y, key1.param[1] + key1.Value, key1.Value, t);
		}


		//======================================================================
		//outgoing()
		//
		//Return the outgoing tangent to the curve at key0.  The value returned
		//for the BEZ2 case is used when extrapolating a linear pre behavior and
		//when interpolating a non-BEZ2 span.
		//====================================================================== 
		static float Outgoing(EnvelopeKey keyp0, EnvelopeKey key0, EnvelopeKey key1)
		{
			float a, b, d, t, _out;

			switch (key0.Shape)
			{
				case KeyShape.ID_TCB:
					{
						a = (1.0f - key0.Tension)
							* (1.0f + key0.Continuity)
							* (1.0f + key0.Bias);
						b = (1.0f - key0.Tension)
							* (1.0f - key0.Continuity)
							* (1.0f - key0.Bias);
						d = key1.Value - key0.Value;

						if (keyp0 != null)
						{
							t = (key1.Time - key0.Time) / (key1.Time - keyp0.Time);
							_out = t * (a * (key0.Value - keyp0.Value) + b * d);
						}
						else
							_out = b * d;
						break;
					}

				case KeyShape.ID_LINE:
					{
						d = key1.Value - key0.Value;
						if (keyp0 != null)
						{
							t = (key1.Time - key0.Time) / (key1.Time - keyp0.Time);
							_out = t * (key0.Value - keyp0.Value + d);
						}
						else
							_out = d;
						break;
					}

				case KeyShape.ID_BEZI:
				case KeyShape.ID_HERM:
					{
						_out = key0.param[1];
						if (keyp0 != null)
							_out *= (key1.Time - key0.Time) / (key1.Time - keyp0.Time);
						break;
					}

				case KeyShape.ID_BEZ2:
					{
						_out = key0.param[3] * (key1.Time - key0.Time);
						if (Math.Abs(key0.param[2]) > 1e-5f)
							_out /= key0.param[2];
						else
							_out *= 1e5f;
						break;
					}

				case KeyShape.ID_STEP:
				default:
					{
						_out = 0.0f;
						break;
					}
			}

			return _out;
		}


		//======================================================================
		//incoming()
		//
		//Return the incoming tangent to the curve at key1.  The value returned
		//for the BEZ2 case is used when extrapolating a linear post behavior.
		//======================================================================
		static float Incoming(EnvelopeKey key0, EnvelopeKey key1, EnvelopeKey keyn1)
		{
			float a, b, d, t, _in;

			switch (key1.Shape)
			{
				case KeyShape.ID_LINE:
				{
					d = key1.Value - key0.Value;
					if (keyn1 != null)
					{
						t = (key1.Time - key0.Time) / (keyn1.Time - key0.Time);
						_in = t * (keyn1.Value - key1.Value + d);
					}
					else
						_in = d;
					break;
				}

				case KeyShape.ID_TCB:
				{
					a = (1.0f - key1.Tension)
						* (1.0f - key1.Continuity)
						* (1.0f + key1.Bias);
					b = (1.0f - key1.Tension)
						* (1.0f + key1.Continuity)
						* (1.0f - key1.Bias);
					d = key1.Value - key0.Value;

					if (keyn1 != null)
					{
						t = (key1.Time - key0.Time) / (keyn1.Time - key0.Time);
						_in = t * (b * (keyn1.Value - key1.Value) + a * d);
					}
					else
						_in = a * d;
					break;
				}

				case KeyShape.ID_BEZI:
				case KeyShape.ID_HERM:
				{
					_in = key1.param[0];
					if (keyn1 != null)
						_in *= (key1.Time - key0.Time) / (keyn1.Time - key0.Time);
					break;
				}

				case KeyShape.ID_BEZ2:
				{
					_in = key1.param[1] * (key1.Time - key0.Time);
					if (Math.Abs(key1.param[0]) > 1e-5f)
						_in /= key1.param[0];
					else
						_in *= 1e5f;
					break;
				}

				case KeyShape.ID_STEP:
				default:
				{
					_in = 0.0f;
					break;
				}
			}

			return _in;
		}

		public float EvaluateEnvelope(float time)
		{
			// if there's no key, the value is 0 
			if (Keys.Count == 0)
				return 0.0f;

			// if there's only one key, the value is constant
			if (Keys.Count == 1)
				return Keys[0].Value;

			// find the first and last keys

			var keyCount = Keys.Count;
			var skey	 = Keys[0];
			var ekey	 = Keys[keyCount - 1];
			var pekey	 = (keyCount >= 2) ? Keys[keyCount - 2] : null;
			var nskey	 = (keyCount >= 2) ? Keys[1] : null;
			
			// use pre-behavior if time is before first key time 
			float	offset = 0, _out = 0, _in = 0;
			int		noff = 0;
			if (time < skey.Time)
			{
				switch (PreBehavior)
				{
					case EvalType.BEH_RESET:
					{
						return 0.0f;
					}

					case EvalType.BEH_CONSTANT:
					{
						return skey.Value;
					}

					case EvalType.BEH_REPEAT:
					{
						int tmp = 0;
						time = Range(time, skey.Time, ekey.Time, ref tmp);
						break;
					}

					case EvalType.BEH_OSCILLATE:
					{
						time = Range(time, skey.Time, ekey.Time, ref noff);
						if ((noff % 2) != 0)
							time = ekey.Time - skey.Time - time;
						break;
					}

					case EvalType.BEH_OFFSET:
					{
						time = Range(time, skey.Time, ekey.Time, ref noff);
						offset = noff * (ekey.Value - skey.Value);
						break;
					}

					case EvalType.BEH_LINEAR:
					{
						_out = Outgoing(null, skey, nskey) / (nskey.Time - skey.Time);
						return _out * (time - skey.Time) + skey.Value;
					}
				}
			}

			// use post-behavior if time is after last key time 

			else if (time > ekey.Time)
			{
				switch (PostBehavior)
				{
					case EvalType.BEH_RESET:
					{
						return 0.0f;
					}

					case EvalType.BEH_CONSTANT:
					{
						return ekey.Value;
					}

					case EvalType.BEH_REPEAT:
					{
						int tmp = 0;
						time = Range(time, skey.Time, ekey.Time, ref tmp);
						break;
					}

					case EvalType.BEH_OSCILLATE:
					{
						time = Range(time, skey.Time, ekey.Time, ref noff);
						if ((noff % 2) != 0)
							time = ekey.Time - skey.Time - time;
						break;
					}

					case EvalType.BEH_OFFSET:
					{
						time = Range(time, skey.Time, ekey.Time, ref noff);
						offset = noff * (ekey.Value - skey.Value);
						break;
					}

					case EvalType.BEH_LINEAR:
					{
						_in = Incoming(pekey, ekey, null) / (ekey.Time - pekey.Time);
						return _in * (time - ekey.Time) + ekey.Value;
					}
				}
			}

			// get the endpoints of the interval being evaluated 

			var index = 0;
			EnvelopeKey key0 = Keys[index];// = key[0];
			while (time > key0.Time)
			{
				index++;
				key0 = Keys[index];
			}
			//while ( time > key0.next.time )
			//	key0 = key0.next;
			EnvelopeKey key1 = Keys[index + 1];
			EnvelopeKey keyp0 = (index > 0) ? Keys[index - 1] : null;
			EnvelopeKey keyn1 = (index + 2 < Keys.Count) ? Keys[index + 2] : null;
			//var key1 = key0.next;

			// check for singularities first 

			if		(time == key0.Time) return key0.Value + offset;
			else if (time == key1.Time) return key1.Value + offset;

			// get interval length, time in [0, 1] 

			var t = (time - key0.Time) / (key1.Time - key0.Time);

			// interpolate
			switch (key1.Shape)
			{
				case KeyShape.ID_TCB:
				case KeyShape.ID_BEZI:
				case KeyShape.ID_HERM:
				{
					_out = Outgoing(keyp0, key0, key1);
					_in = Incoming(key0, key1, keyn1);

					float h1, h2, h3, h4;
					Hermite(t, out h1, out h2, out h3, out h4);
					return h1 * key0.Value + h2 * key1.Value + h3 * _out + h4 * _in + offset;
				}

				case KeyShape.ID_BEZ2: return Bez2(key0, key1, time) + offset;
				case KeyShape.ID_LINE: return key0.Value + t * (key1.Value - key0.Value) + offset;
				case KeyShape.ID_STEP: return key0.Value + offset;

				default:
					return offset;
			}
		}
		#endregion
	};
	#endregion

	#region Clips

	#region ClipType (enum)
	public enum ClipType : uint
	{
		ID_STIL = (uint)(((uint)'S') << 24 | ((uint)'T') << 16 | ((uint)'I') << 8 | ((uint)'L')),
		ID_ISEQ = (uint)(((uint)'I') << 24 | ((uint)'S') << 16 | ((uint)'E') << 8 | ((uint)'Q')),
		ID_ANIM = (uint)(((uint)'A') << 24 | ((uint)'N') << 16 | ((uint)'I') << 8 | ((uint)'M')),
		ID_XREF = (uint)(((uint)'X') << 24 | ((uint)'R') << 16 | ((uint)'E') << 8 | ((uint)'F')),
		ID_STCC = (uint)(((uint)'S') << 24 | ((uint)'T') << 16 | ((uint)'C') << 8 | ((uint)'C'))
	}
	#endregion

	#region ClipDataType (enum)
	enum ClipDataType : uint
	{
		ID_TIME = (uint)(((uint)'T') << 24 | ((uint)'I') << 16 | ((uint)'M') << 8 | ((uint)'E')),
		ID_CLRS = (uint)(((uint)'C') << 24 | ((uint)'L') << 16 | ((uint)'R') << 8 | ((uint)'S')),
		ID_CLRA = (uint)(((uint)'C') << 24 | ((uint)'L') << 16 | ((uint)'R') << 8 | ((uint)'A')),
		ID_FILT = (uint)(((uint)'F') << 24 | ((uint)'I') << 16 | ((uint)'L') << 8 | ((uint)'T')),
		ID_DITH = (uint)(((uint)'D') << 24 | ((uint)'I') << 16 | ((uint)'T') << 8 | ((uint)'H')),
		ID_CONT = (uint)(((uint)'C') << 24 | ((uint)'O') << 16 | ((uint)'N') << 8 | ((uint)'T')),		
		ID_BRIT = (uint)(((uint)'B') << 24 | ((uint)'R') << 16 | ((uint)'I') << 8 | ((uint)'T')),
		ID_SATR = (uint)(((uint)'S') << 24 | ((uint)'A') << 16 | ((uint)'T') << 8 | ((uint)'R')),
		ID_HUE  = (uint)(((uint)'H') << 24 | ((uint)'U') << 16 | ((uint)'E') << 8 | ((uint)' ')),
		ID_GAMM = (uint)(((uint)'G') << 24 | ((uint)'A') << 16 | ((uint)'M') << 8 | ((uint)'M')),
		ID_NEGA = (uint)(((uint)'N') << 24 | ((uint)'E') << 16 | ((uint)'G') << 8 | ((uint)'A')),
		ID_IFLT = (uint)(((uint)'I') << 24 | ((uint)'F') << 16 | ((uint)'L') << 8 | ((uint)'T')),
		ID_PFLT = (uint)(((uint)'P') << 24 | ((uint)'F') << 16 | ((uint)'L') << 8 | ((uint)'T')),

		ID_FLAG = (uint)(((uint)'F') << 24 | ((uint)'L') << 16 | ((uint)'A') << 8 | ((uint)'G')),	// not mentioned in documentation ..
		ID_CROP = (uint)(((uint)'C') << 24 | ((uint)'R') << 16 | ((uint)'O') << 8 | ((uint)'P')),	// not mentioned in documentation ..
		ID_COMP = (uint)(((uint)'C') << 24 | ((uint)'O') << 16 | ((uint)'M') << 8 | ((uint)'P')) 	// not mentioned in documentation ..
	}
	#endregion

	public sealed class ClipStill : Clip
	{
		public ClipStill(uint index) : base(ClipType.ID_STIL, index) { }
		public string			Name;
	};

	public sealed class ClipSequence : Clip
	{
		public ClipSequence(uint index) : base(ClipType.ID_ISEQ, index) { }
		public string			Prefix;              // filename before sequence digits 
		public string			Suffix;              // after digits, e.g. extensions 
		public int				Digits;
		public int				Flags;
		public int				Offset;
		public int				Start;
		public int				End;
	};
	
	public sealed class ClipAnim : Clip
	{
		public ClipAnim(uint index) : base(ClipType.ID_ANIM, index) { }
		public string			Name;
		public string			Server;              // anim loader plug-in 
		public byte[]			Data;
	};
	
	public sealed class ClipCloned : Clip
	{
		public ClipCloned(uint index) : base(ClipType.ID_XREF, index) { }
		public string			Name;					// used to be named 'string'
		public uint				clip_reference_index;	// used to be named 'index'
		public Clip				ClipReference;			// used to be named 'clip' with comment 'ref'
	};

	public sealed class ClipColorCycle : Clip 
	{
		public ClipColorCycle(uint index) : base(ClipType.ID_STCC, index) { }
		public string			Name;
		public int				lo;		// ?
		public int				hi;		// ?
	};

	public abstract class Clip 
	{
		protected Clip(ClipType type, uint index) 
		{ 
			Type				= type;
			Index				= index;

			Contrast.value		= 1.0f;
			Brightness.value	= 1.0f;
			Saturation.value	= 1.0f;
			Gamma.value			= 1.0f;			
		}

		public readonly uint		Index;
		public readonly ClipType	Type;                // ID_STIL, ID_ISEQ, etc. 
		public float	StartTime;
		public float	Duration;
		public float	FrameRate;
		public bool		Negative;
		public readonly EnvelopeParameter	Contrast	= new EnvelopeParameter();
		public readonly EnvelopeParameter	Brightness	= new EnvelopeParameter();
		public readonly EnvelopeParameter	Saturation	= new EnvelopeParameter();
		public readonly EnvelopeParameter	Hue			= new EnvelopeParameter();
		public readonly EnvelopeParameter	Gamma		= new EnvelopeParameter();
		public readonly List<LightwavePlugin>	ImageFilters	= new List<LightwavePlugin>();		// linked list of image filters 
		public readonly List<LightwavePlugin>	PixelFilters	= new List<LightwavePlugin>();		// linked list of pixel filters 
		
		
		public static Clip ReadClip(ChunkReader reader)
		{
			// index 
			var index			= reader.ReadUInt32();

			// first subchunk header 
			var type			= reader.ReadID<ClipType>();
			var subchunk_size	= reader.ReadUInt16();
			subchunk_size += (ushort)(subchunk_size & 1);
			
			Clip clip = null;
			using (var subChunkReader = reader.GetSubChunk(subchunk_size))
			{
				switch (type)
				{
					case ClipType.ID_STIL:
					{
						var tmp_clip	= new ClipStill(index);
						tmp_clip.Name = subChunkReader.ReadString();
						clip = tmp_clip;
						break;
					}

					case ClipType.ID_ISEQ:
					{
						var tmp_clip	= new ClipSequence(index);
						tmp_clip.Digits = subChunkReader.ReadUInt8();
						tmp_clip.Flags	= subChunkReader.ReadUInt8();
						tmp_clip.Offset = subChunkReader.ReadSInt16();
						subChunkReader.ReadUInt16();  // Legacy Cruft: Nothing to see here 
						tmp_clip.Start	= subChunkReader.ReadSInt16();
						tmp_clip.End	= subChunkReader.ReadSInt16();
						tmp_clip.Prefix = subChunkReader.ReadString();
						tmp_clip.Suffix = subChunkReader.ReadString();
						clip = tmp_clip;
						break;
					}

					case ClipType.ID_ANIM:
					{
						var tmp_clip	= new ClipAnim(index);
						tmp_clip.Name	= subChunkReader.ReadString();
						tmp_clip.Server = subChunkReader.ReadString();					
						tmp_clip.Data	= subChunkReader.ReadBytes((uint)subChunkReader.BytesLeft);
						clip = tmp_clip;
						break;
					}

					case ClipType.ID_XREF:
					{
						var tmp_clip			= new ClipCloned(index);
						tmp_clip.clip_reference_index	= subChunkReader.ReadUInt32();
						tmp_clip.Name			= subChunkReader.ReadString();
						clip = tmp_clip;
						break;
					}

					case ClipType.ID_STCC:
					{
						var tmp_clip	= new ClipColorCycle(index);
						tmp_clip.lo		= subChunkReader.ReadSInt16();
						tmp_clip.hi		= subChunkReader.ReadSInt16();
						tmp_clip.Name	= subChunkReader.ReadString();
						clip = tmp_clip;
						break;
					}

					default:
						throw new Exception("Unknown Clip type"); // TODO: create proper exception class for this ...
				}
			}

			while (reader.BytesLeft > 0)
			{
				// process subchunks as they're encountered 
				var id = reader.ReadID<ClipDataType>();
				subchunk_size = reader.ReadUInt16();
				subchunk_size += (ushort)(subchunk_size & 1);
				
				using (var subChunkReader = reader.GetSubChunk(subchunk_size))
				{
					switch (id)
					{
						//case ClipDataType.ID_CLRS: // Color Space RGB   - CLRS { flags[U2], colorspace[U2], filename[FNAM0] }
						//case ClipDataType.ID_CLRA: // Color Space Alpha - CLRA { flags[U2], colorspace[U2], filename[FNAM0] }
						//case ClipDataType.ID_FILT: // Image Filtering   - FILT { flags[U2] }
						//case ClipDataType.ID_DITH: // Image Dithering	  - DITH { flags[U2] }

						// Contrast - CONT { contrast-delta[FP4], envelope[VX] }
						case ClipDataType.ID_CONT: { clip.Contrast.value = subChunkReader.ReadSingle(); clip.Contrast.envelope_index = subChunkReader.ReadVariableLengthIndex(); break; }
					
						// Brightness - BRIT { brightness-delta[FP4], envelope[VX] }
						case ClipDataType.ID_BRIT: { clip.Brightness.value = subChunkReader.ReadSingle(); clip.Brightness.envelope_index = subChunkReader.ReadVariableLengthIndex(); break; }
					
						// Saturation - SATR { saturation-delta[FP4], envelope[VX] }
						case ClipDataType.ID_SATR: { clip.Saturation.value = subChunkReader.ReadSingle(); clip.Saturation.envelope_index = subChunkReader.ReadVariableLengthIndex(); break; }
					
						// Hue - HUE { hue-rotation[FP4], envelope[VX] }
						case ClipDataType.ID_HUE: { clip.Hue.value = subChunkReader.ReadSingle(); clip.Hue.envelope_index = subChunkReader.ReadVariableLengthIndex(); break; }
					
						// Gamma Correction - GAMM { gamma[F4], envelope[VX] }
						case ClipDataType.ID_GAMM: { clip.Gamma.value = subChunkReader.ReadSingle(); clip.Gamma.envelope_index = subChunkReader.ReadVariableLengthIndex(); break; }
					
						// Negative - NEGA { enable[U2] }
						case ClipDataType.ID_NEGA: { clip.Negative = subChunkReader.ReadUInt16() != 0; break; }
						
						// Time - TIME { start-time[FP4], duration[FP4], frame-rate[FP4] }
						case ClipDataType.ID_TIME:
						{
							clip.StartTime			= subChunkReader.ReadSingle();
							clip.Duration			= subChunkReader.ReadSingle();
							clip.FrameRate			= subChunkReader.ReadSingle();
							break;
						}

						// Plug-in Image Filters - IFLT { server-name[S0], flags[U2], data[...] }
						case ClipDataType.ID_IFLT:

						// Plug-in Pixel Filters - PFLT { server-name[S0], flags[U2], data[...] }
						case ClipDataType.ID_PFLT:
						{
							var filt = new LightwavePlugin();
							filt.name	= subChunkReader.ReadString();
							filt.flags	= subChunkReader.ReadUInt16();
							filt.data	= subChunkReader.ReadBytes((uint)subChunkReader.BytesLeft);

							if (id == ClipDataType.ID_IFLT)
								clip.ImageFilters.Add(filt);
							else
								clip.PixelFilters.Add(filt);
							break;
						}

						case ClipDataType.ID_FLAG: // not mentioned in documentation ...
							var flags = subChunkReader.ReadUInt16(); // unknown what they mean ... 
							break;

						default:
							Console.WriteLine("Unknown clip type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return clip;
		}
	};

	#endregion

	#region Textures

	#region TextureCoordinates (enum)
	enum TextureCoordinates : uint
	{
		ID_TMAP = (uint)(((uint)'T') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_AXIS = (uint)(((uint)'A') << 24 | ((uint)'X') << 16 | ((uint)'I') << 8 | ((uint)'S')),
		ID_CNTR = (uint)(((uint)'C') << 24 | ((uint)'N') << 16 | ((uint)'T') << 8 | ((uint)'R')),
		ID_SIZE = (uint)(((uint)'S') << 24 | ((uint)'I') << 16 | ((uint)'Z') << 8 | ((uint)'E')),
		ID_ROTA = (uint)(((uint)'R') << 24 | ((uint)'O') << 16 | ((uint)'T') << 8 | ((uint)'A')),
		ID_OREF = (uint)(((uint)'O') << 24 | ((uint)'R') << 16 | ((uint)'E') << 8 | ((uint)'F')),
		ID_FALL = (uint)(((uint)'F') << 24 | ((uint)'A') << 16 | ((uint)'L') << 8 | ((uint)'L')),
		ID_CSYS = (uint)(((uint)'C') << 24 | ((uint)'S') << 16 | ((uint)'Y') << 8 | ((uint)'S'))
	}
	#endregion

	public class TextureMap 
	{
		public TextureMap()
		{
			Size.values[0] =
			Size.values[1] =
			Size.values[2] = 1.0f;			
		}

		public int		FallType;
		public string	ReferenceObject;
		public int		CoordinateSystem;
		public readonly VectorParameter Size		= new VectorParameter();
		public readonly VectorParameter Center		= new VectorParameter();
		public readonly VectorParameter Rotate		= new VectorParameter();
		public readonly VectorParameter FallOff		= new VectorParameter();

		public static TextureMap ReadTextureMap(ChunkReader reader)
		{
			var tmap = new TextureMap();
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<TextureCoordinates>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);

				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case TextureCoordinates.ID_SIZE:
							tmap.Size.values[0] = subChunkReader.ReadSingle();
							tmap.Size.values[1] = subChunkReader.ReadSingle();
							tmap.Size.values[2] = subChunkReader.ReadSingle();
							tmap.Size.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case TextureCoordinates.ID_CNTR:
							tmap.Size.values[0] = subChunkReader.ReadSingle();
							tmap.Size.values[1] = subChunkReader.ReadSingle();
							tmap.Size.values[2] = subChunkReader.ReadSingle();
							tmap.Center.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case TextureCoordinates.ID_ROTA:
							tmap.Size.values[0] = subChunkReader.ReadSingle();
							tmap.Size.values[1] = subChunkReader.ReadSingle();
							tmap.Size.values[2] = subChunkReader.ReadSingle();
							tmap.Rotate.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case TextureCoordinates.ID_FALL:
							tmap.FallType = subChunkReader.ReadUInt16();
							tmap.Size.values[0] = subChunkReader.ReadSingle();
							tmap.Size.values[1] = subChunkReader.ReadSingle();
							tmap.Size.values[2] = subChunkReader.ReadSingle();
							tmap.FallOff.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case TextureCoordinates.ID_OREF:
							tmap.ReferenceObject = subChunkReader.ReadString();
							break;

						case TextureCoordinates.ID_CSYS:
							tmap.CoordinateSystem = subChunkReader.ReadUInt16();
							break;

						default:
							Console.WriteLine("Unknown texture map type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return tmap;
		}
	};

	#region ProjectionType (enum)
	public enum ProjectionType : ushort
	{
		PROJ_PLANAR       = 0,
		PROJ_CYLINDRICAL  = 1,
		PROJ_SPHERICAL    = 2,
		PROJ_CUBIC        = 3,
		PROJ_FRONT        = 4
	}
	#endregion

	#region WrapType (enum)
	public enum WrapType
	{
		WRAP_NONE    = 0,
		WRAP_EDGE    = 1,
		WRAP_REPEAT  = 2,
		WRAP_MIRROR  = 3
	};
	#endregion

	public class ImageMap 
	{
		public uint				clip_index;
		public ProjectionType	projection;
		public string			vertex_map_name;
		public int				axis;
		public float			aa_strength;
		public int				aas_flags;
		public int				pblend;
		public WrapType			wrapw_type;
		public WrapType			wraph_type;
		public readonly EnvelopeParameter wrapw		= new EnvelopeParameter();
		public readonly EnvelopeParameter wraph		= new EnvelopeParameter();
		public readonly EnvelopeParameter stck		= new EnvelopeParameter();
		public readonly EnvelopeParameter amplitude = new EnvelopeParameter();
	};

	public class ProceduralMap
	{
		public int				axis;
		public float			value_0;
		public float			value_1;
		public float			value_2;
		public string			name;
		public byte[]			data;
	};

	public class GradientKey 
	{
		public float			value;
		public float			rgba_0;
		public float			rgba_1;
		public float			rgba_2;
		public float			rgba_3;
	};

	public class GradientMap 
	{
		public string			paramname;
		public string			itemname;
		public float			start;
		public float			end;
		public int				repeat;
		public GradientKey[]	Keys;			// array of gradient keys 
		public ushort[]			ikey;           // array of interpolation codes 
	};

	#region ImageMapType (enum)
	public enum ImageMapType : uint
	{
		ID_PROJ = (uint)(((uint)'P') << 24 | ((uint)'R') << 16 | ((uint)'O') << 8 | ((uint)'J')),
		ID_STCK = (uint)(((uint)'S') << 24 | ((uint)'T') << 16 | ((uint)'C') << 8 | ((uint)'K')),
		ID_TAMP = (uint)(((uint)'T') << 24 | ((uint)'A') << 16 | ((uint)'M') << 8 | ((uint)'P')),

		ID_TMAP = (uint)(((uint)'T') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_AXIS = (uint)(((uint)'A') << 24 | ((uint)'X') << 16 | ((uint)'I') << 8 | ((uint)'S')),

		ID_VMAP	= (uint)(((uint)'V') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_IMAG = (uint)(((uint)'I') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'G')),
		ID_PIXB = (uint)(((uint)'P') << 24 | ((uint)'I') << 16 | ((uint)'X') << 8 | ((uint)'B')),
		ID_WRAP = (uint)(((uint)'W') << 24 | ((uint)'R') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_WRPW = (uint)(((uint)'W') << 24 | ((uint)'R') << 16 | ((uint)'P') << 8 | ((uint)'W')),
		ID_WRPH = (uint)(((uint)'W') << 24 | ((uint)'R') << 16 | ((uint)'P') << 8 | ((uint)'H')),
		ID_AAST = (uint)(((uint)'A') << 24 | ((uint)'A') << 16 | ((uint)'S') << 8 | ((uint)'T'))
	}
	#endregion

	#region ProceduralType (enum)
	public enum ProceduralType : uint
	{
		ID_TMAP = (uint)(((uint)'T') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_AXIS = (uint)(((uint)'A') << 24 | ((uint)'X') << 16 | ((uint)'I') << 8 | ((uint)'S')),
		ID_VALU = (uint)(((uint)'V') << 24 | ((uint)'A') << 16 | ((uint)'L') << 8 | ((uint)'U')),
		ID_FUNC = (uint)(((uint)'F') << 24 | ((uint)'U') << 16 | ((uint)'N') << 8 | ((uint)'C')),
	}
	#endregion
							
	#region GradientType (enum)
	public enum GradientType : uint
	{
		ID_TMAP = (uint)(((uint)'T') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_AXIS = (uint)(((uint)'A') << 24 | ((uint)'X') << 16 | ((uint)'I') << 8 | ((uint)'S')),
		ID_FUNC = (uint)(((uint)'F') << 24 | ((uint)'U') << 16 | ((uint)'N') << 8 | ((uint)'C')),
		ID_PNAM = (uint)(((uint)'P') << 24 | ((uint)'N') << 16 | ((uint)'A') << 8 | ((uint)'M')),
		ID_INAM = (uint)(((uint)'I') << 24 | ((uint)'N') << 16 | ((uint)'A') << 8 | ((uint)'M')),
		ID_GRST = (uint)(((uint)'G') << 24 | ((uint)'R') << 16 | ((uint)'S') << 8 | ((uint)'T')),
		ID_GRPT = (uint)(((uint)'G') << 24 | ((uint)'R') << 16 | ((uint)'P') << 8 | ((uint)'T')),
		ID_GREN = (uint)(((uint)'G') << 24 | ((uint)'R') << 16 | ((uint)'E') << 8 | ((uint)'N')),
		ID_FKEY = (uint)(((uint)'F') << 24 | ((uint)'K') << 16 | ((uint)'E') << 8 | ((uint)'Y')),
		ID_IKEY = (uint)(((uint)'I') << 24 | ((uint)'K') << 16 | ((uint)'E') << 8 | ((uint)'Y')),
		ID_GVER = (uint)(((uint)'G') << 24 | ((uint)'V') << 16 | ((uint)'E') << 8 | ((uint)'R')), // unknown
	}
	#endregion
			
	#region TextureDataType (enum)
	public enum TextureDataType : uint
	{
		ID_AXIS = (uint)(((uint)'A') << 24 | ((uint)'X') << 16 | ((uint)'I') << 8 | ((uint)'S')),
		ID_FUNC = (uint)(((uint)'F') << 24 | ((uint)'U') << 16 | ((uint)'N') << 8 | ((uint)'C')),
		ID_NEGA = (uint)(((uint)'N') << 24 | ((uint)'E') << 16 | ((uint)'G') << 8 | ((uint)'A')),
		ID_ENAB = (uint)(((uint)'E') << 24 | ((uint)'N') << 16 | ((uint)'A') << 8 | ((uint)'B')),
		ID_OPAC = (uint)(((uint)'O') << 24 | ((uint)'P') << 16 | ((uint)'A') << 8 | ((uint)'C')),
		ID_CHAN = (uint)(((uint)'C') << 24 | ((uint)'H') << 16 | ((uint)'A') << 8 | ((uint)'N')),
	}
	#endregion
		
	#region TextureType (enum)
	public enum TextureType : uint
	{
		ID_TMAP = (uint)(((uint)'T') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_IMAP = (uint)(((uint)'I') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
		ID_PROC = (uint)(((uint)'P') << 24 | ((uint)'R') << 16 | ((uint)'O') << 8 | ((uint)'C')),
		ID_GRAD = (uint)(((uint)'G') << 24 | ((uint)'R') << 16 | ((uint)'A') << 8 | ((uint)'D')),
		ID_SHDR = (uint)(((uint)'S') << 24 | ((uint)'H') << 16 | ((uint)'D') << 8 | ((uint)'R')),
	}
	#endregion

	#region TextureChannel (enum)
	public enum TextureChannel : uint
	{
		ID_COLR = (uint)(((uint)'C') << 24 | ((uint)'O') << 16 | ((uint)'L') << 8 | ((uint)'R')),
		ID_LUMI = (uint)(((uint)'L') << 24 | ((uint)'U') << 16 | ((uint)'M') << 8 | ((uint)'I')),
		ID_DIFF = (uint)(((uint)'D') << 24 | ((uint)'I') << 16 | ((uint)'F') << 8 | ((uint)'F')),
		ID_SPEC = (uint)(((uint)'S') << 24 | ((uint)'P') << 16 | ((uint)'E') << 8 | ((uint)'C')),
		ID_GLOS = (uint)(((uint)'G') << 24 | ((uint)'L') << 16 | ((uint)'O') << 8 | ((uint)'S')),
		ID_REFL = (uint)(((uint)'R') << 24 | ((uint)'E') << 16 | ((uint)'F') << 8 | ((uint)'L')),
		ID_TRAN = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_RIND = (uint)(((uint)'R') << 24 | ((uint)'I') << 16 | ((uint)'N') << 8 | ((uint)'D')),
		ID_TRNL = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'N') << 8 | ((uint)'L')),
		ID_BUMP = (uint)(((uint)'B') << 24 | ((uint)'U') << 16 | ((uint)'M') << 8 | ((uint)'P')),
	}
	#endregion
						

	public class Texture 
	{
		public Texture(TextureType type)
		{
			Type			= type;
			opacity.value	= 1.0f;
			enabled			= 1;
		}

		public readonly TextureType Type;
		public string			ord;
		public TextureChannel	Channel;
		public readonly EnvelopeParameter opacity = new EnvelopeParameter();
		public ushort			opac_type;
		public ushort			enabled;
		public ushort			negative;
		public ushort			axis;
		//union {
			public readonly ImageMap		imap = new ImageMap();
			public readonly ProceduralMap	proc = new ProceduralMap();
			public readonly GradientMap		grad = new GradientMap();
		//}              param;
		public TextureMap	tmap;


		bool ReadImageMap(ChunkReader reader)
		{
			while (reader.BytesLeft > 0)
			{
				// get the next subchunk header 
				var id = reader.ReadID<ImageMapType>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);

				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case ImageMapType.ID_TMAP:
						{
							tmap = TextureMap.ReadTextureMap(subChunkReader);
							break;
						}

						case ImageMapType.ID_PROJ: imap.projection		= (ProjectionType)subChunkReader.ReadUInt16(); break;
						case ImageMapType.ID_VMAP: imap.vertex_map_name = subChunkReader.ReadString(); break;
						case ImageMapType.ID_AXIS: imap.axis			= subChunkReader.ReadUInt16(); break;
						case ImageMapType.ID_IMAG: imap.clip_index		= subChunkReader.ReadVariableLengthIndex(); break;
						case ImageMapType.ID_PIXB: imap.pblend			= subChunkReader.ReadUInt16(); break;

						case ImageMapType.ID_WRAP:
							imap.wrapw_type = (WrapType)subChunkReader.ReadUInt16();
							imap.wraph_type = (WrapType)subChunkReader.ReadUInt16();
							break;

						case ImageMapType.ID_WRPW:
							imap.wrapw.value = subChunkReader.ReadSingle();
							imap.wrapw.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case ImageMapType.ID_WRPH:
							imap.wraph.value = subChunkReader.ReadSingle();
							imap.wraph.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case ImageMapType.ID_AAST:
							imap.aas_flags = subChunkReader.ReadUInt16();
							imap.aa_strength = subChunkReader.ReadSingle();
							break;

						case ImageMapType.ID_STCK:
							imap.stck.value = subChunkReader.ReadSingle();
							imap.stck.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						case ImageMapType.ID_TAMP:
							imap.amplitude.value = subChunkReader.ReadSingle();
							imap.amplitude.envelope_index = subChunkReader.ReadVariableLengthIndex();
							break;

						default:
							Console.WriteLine("Unknown image map type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return true;
		}

		bool ReadProcedural(ChunkReader reader)
		{
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<ProceduralType>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);
				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case ProceduralType.ID_TMAP:
						{
							tmap = TextureMap.ReadTextureMap(subChunkReader);
							break;
						}

						case ProceduralType.ID_AXIS:
						{
							proc.axis = subChunkReader.ReadUInt16();
							break;
						}

						case ProceduralType.ID_VALU:
						{
							proc.value_0 = subChunkReader.ReadSingle();
							if (sz >= 8) proc.value_1 = subChunkReader.ReadSingle();
							if (sz >= 12) proc.value_2 = subChunkReader.ReadSingle();
							break;
						}

						case ProceduralType.ID_FUNC:
						{
							proc.name	= subChunkReader.ReadString();
							proc.data	= subChunkReader.ReadBytes((uint)subChunkReader.BytesLeft);
							break;
						}

						default:
							Console.WriteLine("Unknown procedural map type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return true;
		}

		bool ReadGradient(ChunkReader reader)
		{
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<GradientType>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);

				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						case GradientType.ID_TMAP:
						{
							tmap = TextureMap.ReadTextureMap(subChunkReader);
							break;
						}

						case GradientType.ID_PNAM:
						{
							grad.paramname = subChunkReader.ReadString();
							break;
						}

						case GradientType.ID_INAM:
						{
							grad.itemname = subChunkReader.ReadString();
							break;
						}

						case GradientType.ID_GRST:
						{
							grad.start = subChunkReader.ReadSingle();
							break;
						}

						case GradientType.ID_GREN:
						{
							grad.end = subChunkReader.ReadSingle();
							break;
						}

						case GradientType.ID_GRPT:
						{
							grad.repeat = subChunkReader.ReadUInt16();
							break;
						}

						case GradientType.ID_FKEY:
						{
							var keys = new List<GradientKey>();
							while (subChunkReader.BytesLeft > 0)
							{
								var key = new GradientKey();
								key.value = subChunkReader.ReadSingle();
								key.rgba_0 = subChunkReader.ReadSingle();
								key.rgba_1 = subChunkReader.ReadSingle();
								key.rgba_2 = subChunkReader.ReadSingle();
								key.rgba_3 = subChunkReader.ReadSingle();
								keys.Add(key);
							}
							grad.Keys = keys.ToArray();
							break;
						}

						case GradientType.ID_IKEY:
						{
							var ikeys = new List<ushort>();
							while (subChunkReader.BytesLeft > 0)
								ikeys.Add(subChunkReader.ReadUInt16());
							grad.ikey = ikeys.ToArray();
							break;
						}

						case GradientType.ID_GVER:	// unknown, not mentioned in lightwave sdk documentation
							break;

						default:
							Console.WriteLine("Unknown gradient type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}
			return true;
		}

		public static Texture ReadTexture(ChunkReader reader, TextureType type)
		{
			var tex			= new Texture(type);
			var header_size = reader.ReadUInt16();			
			using (var headerReader = reader.GetSubChunk(header_size))
			{
				// ordinal string 
				tex.ord = headerReader.ReadString();

				// process subchunks as they're encountered 
				while (headerReader.BytesLeft > 0)
				{
					var subchunk_id		= headerReader.ReadID<TextureDataType>();
					var subchunk_size	= headerReader.ReadUInt16();
					subchunk_size += (ushort)(subchunk_size & 1);
					using (var subChunkReader = headerReader.GetSubChunk(subchunk_size))
					{
						switch (subchunk_id)
						{
							case TextureDataType.ID_CHAN:
								tex.Channel = subChunkReader.ReadID<TextureChannel>();
								break;

							case TextureDataType.ID_OPAC:
								tex.opac_type = subChunkReader.ReadUInt16();
								tex.opacity.value = subChunkReader.ReadSingle();
								tex.opacity.envelope_index = subChunkReader.ReadVariableLengthIndex();
								break;

							case TextureDataType.ID_ENAB:
								tex.enabled = subChunkReader.ReadUInt16();
								break;

							case TextureDataType.ID_NEGA:
								tex.negative = subChunkReader.ReadUInt16();
								break;

							case TextureDataType.ID_AXIS:
								tex.axis = subChunkReader.ReadUInt16();
								break;

							default:
								Console.WriteLine("Unknown texture header type " + reader.GetIDString((uint)subchunk_id));
								break;
						}
					}
				}
			}

			if (reader.BytesLeft > 0)
			{
				using (var blockReader = reader.GetSubChunk((uint)reader.BytesLeft))
				{
					switch (type)
					{
						case TextureType.ID_IMAP: tex.ReadImageMap(blockReader); break;
						case TextureType.ID_PROC: tex.ReadProcedural(blockReader); break;
						case TextureType.ID_GRAD: tex.ReadGradient(blockReader); break;
						default:
							Console.WriteLine("Unknown texture type " + reader.GetIDString((uint)type));
							break;
					}
				}
			}
			return tex;
		}
	};


	// values that can be textured - Q: what the hell does that even mean?
	public class TextureParameter
	{
		public float Value;
		public uint	 envelope_index;	
		public readonly List<Texture>	textures = new List<Texture>();	// linked list of texture layers 
	};

	public class ClipParameter
	{
		public float Red;
		public float Green;
		public float Blue;
		public uint	 envelope_index;
		public readonly List<Texture>	textures = new List<Texture>();	// linked list of texture layers 
	};

	#endregion

	#region Surfaces

	// surfaces 
	public class lwGlow // Q: never used? 
	{
		public short				enabled;
		public short				type;
		public readonly EnvelopeParameter	intensity	= new EnvelopeParameter();
		public readonly EnvelopeParameter	size		= new EnvelopeParameter();
	};

	public class lwRMap // Q: what does this mean?
	{
		public readonly TextureParameter	values		= new TextureParameter();
		public int					options;
		public uint					clip_index;
		public float				seam_angle;
	};

	public class lwLine // Q: line??
	{
		public short				enabled;
		public ushort				flags;
		public readonly EnvelopeParameter	size		= new EnvelopeParameter();
	};

	#region SurfaceParameter (enum)
	enum SurfaceParameter : uint
	{
		ID_VCOL = (uint)(((uint)'V') << 24 | ((uint)'C') << 16 | ((uint)'O') << 8 | ((uint)'L')),
		ID_COLR = (uint)(((uint)'C') << 24 | ((uint)'O') << 16 | ((uint)'L') << 8 | ((uint)'R')),
		ID_LUMI = (uint)(((uint)'L') << 24 | ((uint)'U') << 16 | ((uint)'M') << 8 | ((uint)'I')),
		ID_VLUM = (uint)(((uint)'V') << 24 | ((uint)'L') << 16 | ((uint)'U') << 8 | ((uint)'M')),	// LWOB
		ID_DIFF = (uint)(((uint)'D') << 24 | ((uint)'I') << 16 | ((uint)'F') << 8 | ((uint)'F')),
		ID_VDIF = (uint)(((uint)'V') << 24 | ((uint)'D') << 16 | ((uint)'I') << 8 | ((uint)'F')),	// LWOB
		ID_SPEC = (uint)(((uint)'S') << 24 | ((uint)'P') << 16 | ((uint)'E') << 8 | ((uint)'C')),
		ID_VSPC = (uint)(((uint)'V') << 24 | ((uint)'S') << 16 | ((uint)'P') << 8 | ((uint)'C')),	// LWOB
		ID_GLOS = (uint)(((uint)'G') << 24 | ((uint)'L') << 16 | ((uint)'O') << 8 | ((uint)'S')),
		ID_REFL = (uint)(((uint)'R') << 24 | ((uint)'E') << 16 | ((uint)'F') << 8 | ((uint)'L')),
		ID_RFLT = (uint)(((uint)'R') << 24 | ((uint)'F') << 16 | ((uint)'L') << 8 | ((uint)'T')),	// LWOB
		ID_RFOP = (uint)(((uint)'R') << 24 | ((uint)'F') << 16 | ((uint)'O') << 8 | ((uint)'P')),
		ID_RIMG = (uint)(((uint)'R') << 24 | ((uint)'I') << 16 | ((uint)'M') << 8 | ((uint)'G')),
		ID_RBLR = (uint)(((uint)'R') << 24 | ((uint)'B') << 16 | ((uint)'L') << 8 | ((uint)'R')),
		ID_RSAN = (uint)(((uint)'R') << 24 | ((uint)'S') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_TRAN = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_TROP = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'O') << 8 | ((uint)'P')),
		ID_TIMG = (uint)(((uint)'T') << 24 | ((uint)'I') << 16 | ((uint)'M') << 8 | ((uint)'G')),
		ID_RIND = (uint)(((uint)'R') << 24 | ((uint)'I') << 16 | ((uint)'N') << 8 | ((uint)'D')),			
		ID_TRNL = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'N') << 8 | ((uint)'L')),
		ID_BUMP = (uint)(((uint)'B') << 24 | ((uint)'U') << 16 | ((uint)'M') << 8 | ((uint)'P')),
		ID_SMAN = (uint)(((uint)'S') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'N')),
		ID_SIDE = (uint)(((uint)'S') << 24 | ((uint)'I') << 16 | ((uint)'D') << 8 | ((uint)'E')),
		ID_CLRH = (uint)(((uint)'C') << 24 | ((uint)'L') << 16 | ((uint)'R') << 8 | ((uint)'H')),
		ID_CLRF = (uint)(((uint)'C') << 24 | ((uint)'L') << 16 | ((uint)'R') << 8 | ((uint)'F')),
		ID_ADTR = (uint)(((uint)'A') << 24 | ((uint)'D') << 16 | ((uint)'T') << 8 | ((uint)'R')),
		ID_SHRP = (uint)(((uint)'S') << 24 | ((uint)'H') << 16 | ((uint)'R') << 8 | ((uint)'P')),
		ID_LINE = (uint)(((uint)'L') << 24 | ((uint)'I') << 16 | ((uint)'N') << 8 | ((uint)'E')),
		ID_LSIZ = (uint)(((uint)'L') << 24 | ((uint)'S') << 16 | ((uint)'I') << 8 | ((uint)'Z')),
		ID_ALPH = (uint)(((uint)'A') << 24 | ((uint)'L') << 16 | ((uint)'P') << 8 | ((uint)'H')),
		ID_AVAL = (uint)(((uint)'A') << 24 | ((uint)'V') << 16 | ((uint)'A') << 8 | ((uint)'L')),
		ID_GVAL = (uint)(((uint)'G') << 24 | ((uint)'V') << 16 | ((uint)'A') << 8 | ((uint)'L')),
		ID_BLOK = (uint)(((uint)'B') << 24 | ((uint)'L') << 16 | ((uint)'O') << 8 | ((uint)'K')),	
		ID_GLOW = (uint)(((uint)'G') << 24 | ((uint)'L') << 16 | ((uint)'O') << 8 | ((uint)'W')),
		ID_LCOL = (uint)(((uint)'L') << 24 | ((uint)'C') << 16 | ((uint)'O') << 8 | ((uint)'L')),
		ID_CMNT = (uint)(((uint)'C') << 24 | ((uint)'M') << 16 | ((uint)'N') << 8 | ((uint)'T')),	// unknowm
		ID_VERS = (uint)(((uint)'V') << 24 | ((uint)'E') << 16 | ((uint)'R') << 8 | ((uint)'S')),	// unknown
		ID_NODS = (uint)(((uint)'N') << 24 | ((uint)'O') << 16 | ((uint)'D') << 8 | ((uint)'S')),	// unknown
		ID_TVEL = (uint)(((uint)'T') << 24 | ((uint)'V') << 16 | ((uint)'E') << 8 | ((uint)'L')),	// LWOB
		ID_TFAL = (uint)(((uint)'T') << 24 | ((uint)'F') << 16 | ((uint)'A') << 8 | ((uint)'L')),	// LWOB
		ID_TVAL = (uint)(((uint)'T') << 24 | ((uint)'V') << 16 | ((uint)'A') << 8 | ((uint)'L')),	// LWOB
		ID_TAMP = (uint)(((uint)'T') << 24 | ((uint)'A') << 16 | ((uint)'M') << 8 | ((uint)'P')),	// LWOB
		ID_STEX = (uint)(((uint)'S') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_TTEX = (uint)(((uint)'T') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_TSIZ = (uint)(((uint)'T') << 24 | ((uint)'S') << 16 | ((uint)'I') << 8 | ((uint)'Z')),	// LWOB
		ID_TREF = (uint)(((uint)'T') << 24 | ((uint)'R') << 16 | ((uint)'E') << 8 | ((uint)'F')),	// LWOB
		ID_TOPC = (uint)(((uint)'T') << 24 | ((uint)'O') << 16 | ((uint)'P') << 8 | ((uint)'C')),	// LWOB
		ID_TFP0 = (uint)(((uint)'T') << 24 | ((uint)'F') << 16 | ((uint)'P') << 8 | ((uint)'0')),	// LWOB
		ID_TFP1 = (uint)(((uint)'T') << 24 | ((uint)'F') << 16 | ((uint)'P') << 8 | ((uint)'1')),	// LWOB
		ID_TFLG = (uint)(((uint)'T') << 24 | ((uint)'F') << 16 | ((uint)'L') << 8 | ((uint)'G')),	// LWOB
		ID_TCTR = (uint)(((uint)'T') << 24 | ((uint)'C') << 16 | ((uint)'T') << 8 | ((uint)'R')),	// LWOB
		ID_TCLR = (uint)(((uint)'T') << 24 | ((uint)'C') << 16 | ((uint)'L') << 8 | ((uint)'R')),	// LWOB
		ID_TAAS = (uint)(((uint)'T') << 24 | ((uint)'A') << 16 | ((uint)'A') << 8 | ((uint)'S')),	// LWOB
		ID_SDAT = (uint)(((uint)'S') << 24 | ((uint)'D') << 16 | ((uint)'A') << 8 | ((uint)'T')),	// LWOB
		ID_LTEX = (uint)(((uint)'L') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_RTEX = (uint)(((uint)'R') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_BTEX = (uint)(((uint)'B') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_CTEX = (uint)(((uint)'C') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_DTEX = (uint)(((uint)'D') << 24 | ((uint)'T') << 16 | ((uint)'E') << 8 | ((uint)'X')),	// LWOB
		ID_FLAG = (uint)(((uint)'F') << 24 | ((uint)'L') << 16 | ((uint)'A') << 8 | ((uint)'G')),	// LWOB
		ID_SHDR = (uint)(((uint)'S') << 24 | ((uint)'H') << 16 | ((uint)'D') << 8 | ((uint)'R')),	// LWOB
	}
	#endregion

	public class Surface 
	{
		public Surface()
		{
			color.Red			= 0.78431f;
			color.Green			= 0.78431f;
			color.Blue			= 0.78431f;
			diffuse.Value		= 1.0f;
			glossiness.Value	= 0.4f;
			bump.Value			= 1.0f;
			eta.Value			= 1.0f;
			sideflags			= 1;
		}

		public string						name;
		public string						srcname;
		public float						smooth;
		public int							sideflags;
		public float						alpha;
		public int							alpha_mode;
		public readonly lwLine				line			= new lwLine();
		public readonly ClipParameter		color			= new ClipParameter();
		public readonly EnvelopeParameter	color_hilite	= new EnvelopeParameter();
		public readonly EnvelopeParameter	color_filter	= new EnvelopeParameter();
		public readonly EnvelopeParameter	add_trans		= new EnvelopeParameter();
		public readonly EnvelopeParameter	dif_sharp		= new EnvelopeParameter();
		public readonly EnvelopeParameter	glow			= new EnvelopeParameter();
		public readonly TextureParameter	luminosity		= new TextureParameter();
		public readonly TextureParameter	diffuse			= new TextureParameter();
		public readonly TextureParameter	specularity		= new TextureParameter();
		public readonly TextureParameter	glossiness		= new TextureParameter(); 
		public readonly lwRMap				reflection		= new lwRMap();
		public readonly lwRMap				transparency	= new lwRMap();
		public readonly TextureParameter	eta				= new TextureParameter();
		public readonly TextureParameter	translucency	= new TextureParameter();
		public readonly TextureParameter	bump			= new TextureParameter();
		
		public readonly List<LightwavePlugin> shader = new List<LightwavePlugin>();              // linked list of shaders 
		

		public static Surface ReadSurface(ChunkReader reader)
		{
			// allocate the Surface structure 
			var surf = new Surface();
			
			// names 
			surf.name		= reader.ReadString();
			surf.srcname	= reader.ReadString();
			
			// process subchunks as they're encountered 
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<SurfaceParameter>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);
				
				using (var subChunkReader = reader.GetSubChunk(sz))
				{
					switch (id)
					{
						// Vertex Color Map - VCOL { intensity[FP4], envelope[VX], vmap-type[ID4], name[S0] }
						//		The vertex color map subchunk identifies an RGB or RGBA VMAP that will be used to color the surface
						case SurfaceParameter.ID_VCOL: 
						{
							var intensity		= subChunkReader.ReadSingle();
							var envelope_index	= subChunkReader.ReadVariableLengthIndex();
							var vmap_type		= subChunkReader.ReadUInt32();
							var name			= subChunkReader.ReadString();							
							break;
						}

						case SurfaceParameter.ID_CMNT: // Not mentioned in LWO documentation, maybe means 'comment'?
						{
							break;
						}

						case SurfaceParameter.ID_VERS: // Not mentioned in LWO documentation, maybe means 'version'?
						{
							break;
						}

						case SurfaceParameter.ID_NODS: // Not mentioned in LWO documentation, maybe means 'nodes'?
						{
							break;
						}
						
						case SurfaceParameter.ID_COLR:
						{
							surf.color.Red							= subChunkReader.ReadSingle();
							surf.color.Green						= subChunkReader.ReadSingle();
							surf.color.Blue							= subChunkReader.ReadSingle();
							surf.color.envelope_index				= subChunkReader.ReadVariableLengthIndex();
							break;
						}

						case SurfaceParameter.ID_LUMI:	
							surf.luminosity.Value					= subChunkReader.ReadSingle();
							surf.luminosity.envelope_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_DIFF:
							surf.diffuse.Value						= subChunkReader.ReadSingle();
							surf.diffuse.envelope_index				= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_SPEC:
							surf.specularity.Value					= subChunkReader.ReadSingle();
							surf.specularity.envelope_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_GLOS:
							surf.glossiness.Value					= subChunkReader.ReadSingle();
							surf.glossiness.envelope_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_REFL:
							surf.reflection.values.Value			= subChunkReader.ReadSingle();
							surf.reflection.values.envelope_index	= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_RFOP:
							surf.reflection.options					= subChunkReader.ReadUInt16();
							break;

						case SurfaceParameter.ID_RIMG:
							surf.reflection.clip_index				= subChunkReader.ReadVariableLengthIndex();
							break;

						// Reflection Blurring - RBLR { blur-percentage[FP4], envelope[VX] }
						//		The amount of blurring of reflections. The default is zero.
						case SurfaceParameter.ID_RBLR:
							break;

						case SurfaceParameter.ID_RSAN:
							surf.reflection.seam_angle				= subChunkReader.ReadSingle();
							break;

						case SurfaceParameter.ID_TRAN:
							surf.transparency.values.Value			= subChunkReader.ReadSingle();
							surf.transparency.values.envelope_index	= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_TROP:
							surf.transparency.options				= subChunkReader.ReadUInt16();
							break;

						case SurfaceParameter.ID_TIMG:
							surf.transparency.clip_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_RIND:
							surf.eta.Value							= subChunkReader.ReadSingle();
							surf.eta.envelope_index					= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_TRNL:
							surf.translucency.Value					= subChunkReader.ReadSingle();
							surf.translucency.envelope_index		= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_BUMP:
							surf.bump.Value							= subChunkReader.ReadSingle();
							surf.bump.envelope_index				= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_SMAN:
							surf.smooth								= subChunkReader.ReadSingle();
							break;

						case SurfaceParameter.ID_SIDE:
							surf.sideflags							= subChunkReader.ReadUInt16();
							break;

						case SurfaceParameter.ID_CLRH:
							surf.color_hilite.value					= subChunkReader.ReadSingle();
							surf.color_hilite.envelope_index		= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_CLRF:
							surf.color_filter.value					= subChunkReader.ReadSingle();
							surf.color_filter.envelope_index		= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_ADTR:
							surf.add_trans.value					= subChunkReader.ReadSingle();
							surf.add_trans.envelope_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_SHRP:
							surf.dif_sharp.value					= subChunkReader.ReadSingle();
							surf.dif_sharp.envelope_index			= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_GVAL:
							surf.glow.value							= subChunkReader.ReadSingle();
							surf.glow.envelope_index				= subChunkReader.ReadVariableLengthIndex();
							break;

						// Render Outlines - LINE { flags[U2], ( size[F4], size-envelope[VX], ( color[COL12], color-envelope[VX] )? )? }
						case SurfaceParameter.ID_LINE:
							surf.line.enabled = 1;
							if (sz >= 2) surf.line.flags				= subChunkReader.ReadUInt16();
							if (sz >= 6) surf.line.size.value			= subChunkReader.ReadSingle();
							if (sz >= 8) surf.line.size.envelope_index	= subChunkReader.ReadVariableLengthIndex();
							break;

						case SurfaceParameter.ID_ALPH:
							surf.alpha_mode = subChunkReader.ReadUInt16();
							surf.alpha		= subChunkReader.ReadSingle();
							break;

						case SurfaceParameter.ID_AVAL:
							surf.alpha		= subChunkReader.ReadSingle();
							break;

						case SurfaceParameter.ID_BLOK:
						{
							var type = subChunkReader.ReadID<TextureType>();
							switch (type)
							{
								case TextureType.ID_IMAP:
								case TextureType.ID_PROC:
								case TextureType.ID_GRAD:
								{
									using (var blockReader = subChunkReader.GetSubChunk((uint)subChunkReader.BytesLeft))
									{
										var tex = Texture.ReadTexture(blockReader, type);									
										switch ( tex.Channel ) 
										{
											case TextureChannel.ID_COLR: surf.color					.textures.Add(tex); break;
											case TextureChannel.ID_LUMI: surf.luminosity			.textures.Add(tex); break;
											case TextureChannel.ID_DIFF: surf.diffuse				.textures.Add(tex); break;
											case TextureChannel.ID_SPEC: surf.specularity			.textures.Add(tex); break;
											case TextureChannel.ID_GLOS: surf.glossiness			.textures.Add(tex); break;
											case TextureChannel.ID_REFL: surf.reflection   .values	.textures.Add(tex); break;
											case TextureChannel.ID_TRAN: surf.transparency .values	.textures.Add(tex); break;
											case TextureChannel.ID_RIND: surf.eta					.textures.Add(tex); break;
											case TextureChannel.ID_TRNL: surf.translucency			.textures.Add(tex); break;
											case TextureChannel.ID_BUMP: surf.bump					.textures.Add(tex); break;
											default:
												throw new Exception("Unknown texture channel"); // TODO: create proper exception calass for this
										}
									}
									break;
								}
								case TextureType.ID_SHDR:
								{
									using (var blockReader = subChunkReader.GetSubChunk((uint)subChunkReader.BytesLeft))
									{
										surf.shader.Add(
												LightwavePlugin.ReadShader(blockReader)
											);
									}
									break;
								}
								default:
									Console.WriteLine("Unknown blok type " + reader.GetIDString((uint)type));
									break;
							}
							break;
						}

						default:
							Console.WriteLine("Unknown surface parameter type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			return surf;
		}

		#region ParseTexture
		//======================================================================
		// ParseTexture()
		//
		// Create a new texture for BTEX, CTEX, etc. subchunks.
		//====================================================================== 
		static Texture ParseTexture(string src)
		{
			var pos = src.IndexOf("Image Map");
			if (pos != -1)
			{
				var tex = new Texture(TextureType.ID_IMAP);
				
				if		(src.Contains("Planar"))		tex.imap.projection = ProjectionType.PROJ_PLANAR;
				else if (src.Contains("Cylindrical"))	tex.imap.projection = ProjectionType.PROJ_CYLINDRICAL;
				else if (src.Contains("Spherical"))		tex.imap.projection = ProjectionType.PROJ_SPHERICAL;
				else if (src.Contains("Cubic"))			tex.imap.projection = ProjectionType.PROJ_CUBIC;
				else if (src.Contains("Front"))			tex.imap.projection = ProjectionType.PROJ_FRONT;
				tex.imap.aa_strength = 1.0f;
				tex.imap.amplitude.value = 1.0f;
				return tex;
			} else
			{
				var tex = new Texture(TextureType.ID_PROC);
				tex.proc.name = src;
				return tex;
			}

		}
		#endregion

		#region AddTextureVelocity
		//======================================================================
		// AddTextureVelocity()
		//
		// Add a triple of envelopes to simulate the old texture velocity
		// parameters.
		//====================================================================== 
		static uint AddTextureVelocity(float[] pos, float[] vel, List<Envelope> elist)
		{
			Envelope env = null;
			for (var i = 0; i < 3; i++)
			{
				env = new Envelope();
				var key0 = new EnvelopeKey(0,					// time
									pos[i]);					// value
				var key1 = new EnvelopeKey(1,					// time
									 pos[i] + vel[i] * 30.0f);	// value

				key0.Shape = key1.Shape = KeyShape.ID_LINE;

				env.Index = (uint)(elist.Count + 1); // Q: shouldn't this be + 0?
				env.Type = 0x0301 + i;
				env.Name = "Position." + i;
				env.Keys.Add(key0);
				env.Keys.Add(key1);
				env.PreBehavior		= EvalType.BEH_LINEAR;
				env.PostBehavior	= EvalType.BEH_LINEAR;

				elist.Add(env);
			}

			return env.Index - 2; // Q: why the -2?
		}
		#endregion

		#region AddClip
		//======================================================================
		// AddClip()
		//
		// Add a clip to the clip list.  Used to store the contents of an RIMG or
		// TIMG surface subchunk.
		//====================================================================== 
		static uint AddClip(string src, List<Clip> clist)
		{
			int pos = src.IndexOf("(sequence)");
			if (pos != -1)
			{
				var clip = new ClipSequence((uint)clist.Count);
				clip.Prefix = src.Substring(pos);
				clip.Digits = 3;
				clist.Add(clip);
				return clip.Index;
			} else
			{
				var clip = new ClipStill((uint)clist.Count);
				clip.Name = src;
				clist.Add(clip);
				return clip.Index;
			}
		}
		#endregion


		// LWOB
		public static Surface ReadSurface5(ChunkReader reader, LightwaveObject obj)
		{
			var surf = new Surface();
			surf.name = reader.ReadString();

			Texture tex = null;
			LightwavePlugin shdr = null;
			float[] v = new float[3];
			uint flags = 0;

			// process subchunks as they're encountered 
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<SurfaceParameter>();
				var sz = reader.ReadUInt16();
				sz += (ushort)(sz & 1);

				using (var subChunkReader = reader.GetSubChunk(sz))
 				{
					switch (id)
					{
						case SurfaceParameter.ID_COLR:
						{
							surf.color.Red = subChunkReader.ReadUInt8() / 255.0f;
							surf.color.Green = subChunkReader.ReadUInt8() / 255.0f;
							surf.color.Blue = subChunkReader.ReadUInt8() / 255.0f;
							break;
						}

						case SurfaceParameter.ID_FLAG:
						{
							flags = subChunkReader.ReadUInt16();
							if ((flags & 4) != 0) surf.smooth = 1.56207f;
							if ((flags & 8) != 0) surf.color_hilite.value = 1.0f;
							if ((flags & 16) != 0) surf.color_filter.value = 1.0f;
							if ((flags & 128) != 0) surf.dif_sharp.value = 0.5f;
							if ((flags & 256) != 0) surf.sideflags = 3;
							if ((flags & 512) != 0) surf.add_trans.value = 1.0f;
							break;
						}

						case SurfaceParameter.ID_LUMI:
						{
							surf.luminosity.Value = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_VLUM:
						{
							surf.luminosity.Value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_DIFF:
						{
							surf.diffuse.Value = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_VDIF:
						{
							surf.diffuse.Value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_SPEC:
						{
							surf.specularity.Value = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_VSPC:
						{
							surf.specularity.Value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_GLOS:
						{
							surf.glossiness.Value = (float)Math.Log(subChunkReader.ReadUInt16()) / 20.7944f;
							break;
						}

						case SurfaceParameter.ID_SMAN:
						{
							surf.smooth = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_REFL:
						{
							surf.reflection.values.Value = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_RFLT:
						{
							surf.reflection.options = subChunkReader.ReadUInt16();
							break;
						}

						case SurfaceParameter.ID_RIMG:
						{
							var s = subChunkReader.ReadString();
							surf.reflection.clip_index = AddClip(s, obj.Clips);
							surf.reflection.options = 3;
							break;
						}

						case SurfaceParameter.ID_RSAN:
						{
							surf.reflection.seam_angle = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TRAN:
						{
							surf.transparency.values.Value = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_RIND:
						{
							surf.eta.Value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_BTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.bump.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_CTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.color.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_DTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.diffuse.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_LTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.luminosity.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_RTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.reflection.values.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_STEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.specularity.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_TTEX:
						{
							var s = subChunkReader.ReadString(sz);//.getbytes((uint)sz);
							tex = ParseTexture(s);
							surf.transparency.values.textures.Add(tex);
							break;
						}

						case SurfaceParameter.ID_TFLG:
						{
							if (tex == null)
								return null;
							flags = subChunkReader.ReadUInt16();

							int i = 0;
							if ((flags & 1) != 0) i = 0;
							if ((flags & 2) != 0) i = 1;
							if ((flags & 4) != 0) i = 2;
							tex.axis = (ushort)i;
							if (tex.Type == TextureType.ID_IMAP)
								tex.imap.axis = i;
							else
								tex.proc.axis = i;

							if ((flags & 8) != 0) tex.tmap.CoordinateSystem = 1;
							if ((flags & 16) != 0) tex.negative = 1;
							if ((flags & 32) != 0) tex.imap.pblend = 1;
							if ((flags & 64) != 0)
							{
								tex.imap.aa_strength = 1.0f;
								tex.imap.aas_flags = 1;
							}
							break;
						}

						case SurfaceParameter.ID_TSIZ:
						{
							tex.tmap.Size.values[0] = subChunkReader.ReadSingle();
							tex.tmap.Size.values[1] = subChunkReader.ReadSingle();
							tex.tmap.Size.values[2] = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TCTR:
						{
							tex.tmap.Center.values[0] = subChunkReader.ReadSingle();
							tex.tmap.Center.values[1] = subChunkReader.ReadSingle();
							tex.tmap.Center.values[2] = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TFAL:
						{
							tex.tmap.FallOff.values[0] = subChunkReader.ReadSingle();
							tex.tmap.FallOff.values[1] = subChunkReader.ReadSingle();
							tex.tmap.FallOff.values[2] = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TVEL:
						{
							for (var i = 0; i < 3; i++)
								v[i] = subChunkReader.ReadSingle();
							tex.tmap.Center.envelope_index = AddTextureVelocity(tex.tmap.Center.values, v, obj.Envelopes);
							break;
						}

						case SurfaceParameter.ID_TCLR:
						{
							if (tex.Type == TextureType.ID_PROC)
							{
								tex.proc.value_0 = subChunkReader.ReadUInt8() / 255.0f;
								tex.proc.value_1 = subChunkReader.ReadUInt8() / 255.0f;
								tex.proc.value_2 = subChunkReader.ReadUInt8() / 255.0f;
							}
							break;
						}

						case SurfaceParameter.ID_TVAL:
						{
							tex.proc.value_0 = subChunkReader.ReadSInt16() / 256.0f;
							break;
						}

						case SurfaceParameter.ID_TAMP:
						{
							if (tex.Type == TextureType.ID_IMAP)
								tex.imap.amplitude.value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TIMG:
						{
							var s = subChunkReader.ReadString();
							tex.imap.clip_index = AddClip(s, obj.Clips);
							break;
						}

						case SurfaceParameter.ID_TAAS:
						{
							tex.imap.aa_strength = subChunkReader.ReadSingle();
							tex.imap.aas_flags = 1;
							break;
						}

						case SurfaceParameter.ID_TREF:
						{
							tex.tmap.ReferenceObject = subChunkReader.ReadString((uint)sz);//.getbytes((uint)sz);
							break;
						}

						case SurfaceParameter.ID_TOPC:
						{
							tex.opacity.value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TFP0:
						{
							if (tex.Type == TextureType.ID_IMAP)
								tex.imap.wrapw.value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_TFP1:
						{
							if (tex.Type == TextureType.ID_IMAP)
								tex.imap.wraph.value = subChunkReader.ReadSingle();
							break;
						}

						case SurfaceParameter.ID_SHDR:
						{
							shdr = new LightwavePlugin();
							if (shdr == null)
								return null;
							shdr.name = subChunkReader.ReadString((uint)sz);
							surf.shader.Add(shdr);
							break;
						}

						case SurfaceParameter.ID_SDAT:
						{
							if (shdr == null)
								return null;
							shdr.data = subChunkReader.ReadBytes(sz);
							break;
						}

						default:
							Console.WriteLine("Unknown surface parameter type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}
			return surf;
		}
	};

	#endregion

	#region Vertex Maps

	#region VertexMapType (enum)
	public enum VertexMapType : uint
	{
		ID_PICK = (uint)(((uint)'P') << 24 | ((uint)'I') << 16 | ((uint)'C') << 8 | ((uint)'K')),
		ID_WGHT = (uint)(((uint)'W') << 24 | ((uint)'G') << 16 | ((uint)'H') << 8 | ((uint)'T')),
		ID_MNVW = (uint)(((uint)'M') << 24 | ((uint)'N') << 16 | ((uint)'V') << 8 | ((uint)'W')),
		ID_TXUV = (uint)(((uint)'T') << 24 | ((uint)'X') << 16 | ((uint)'U') << 8 | ((uint)'V')),
		ID_RGB  = (uint)(((uint)'R') << 24 | ((uint)'G') << 16 | ((uint)'B') << 8 | ((uint)' ')),
		ID_RGBA = (uint)(((uint)'R') << 24 | ((uint)'G') << 16 | ((uint)'B') << 8 | ((uint)'A')),
		ID_MORF = (uint)(((uint)'M') << 24 | ((uint)'O') << 16 | ((uint)'R') << 8 | ((uint)'F')),
		ID_SPOT = (uint)(((uint)'S') << 24 | ((uint)'P') << 16 | ((uint)'O') << 8 | ((uint)'T')),
	}
	#endregion

	public class VertexMapPoint 
	{
		public VertexMapPoint(VertexMap vertex_map, uint index) { VertexMap = vertex_map; Index = index; }
		public VertexMap		VertexMap;
		public uint				Index;               // vertex_index or polygon_index element 
	};

	// vertex maps 
	public class VertexMap 
	{
		public VertexMap(VertexMapType type, string name, bool perpoly) 
		{
			Type		= type;
			Name		= name;
			PerPoly		= perpoly; 
		}

		public readonly string			Name;
		public readonly VertexMapType	Type;
		public readonly bool			PerPoly;
		public float[][]				Values;

		public uint[] vertex_index;		// array of point indexes 
		public uint[] polygon_index;	// array of polygon indexes 

		public static VertexMap ReadVertexMap(ChunkReader reader, bool perpoly)
		{
			var type		= reader.ReadID<VertexMapType>();
			var dimensions	= reader.ReadUInt16();
			var name		= reader.ReadString();

			var vertex_map	= new VertexMap(type, name, perpoly);
			
			var vertex_indices	= new List<uint>();
			var polygon_indices	= new List<uint>();
			var values			= new List<float[]>();
			
			// fill in the vertex-map values 
			while (reader.BytesLeft > 0)
			{
				vertex_indices.Add(reader.ReadVariableLengthIndex());
				if (perpoly)
					polygon_indices.Add(reader.ReadVariableLengthIndex());
				
				var pointValues = new float[dimensions];
				for (var j = 0; j < dimensions; j++)
					pointValues[j] = reader.ReadSingle();
				values.Add(pointValues);
			}
			vertex_map.Values = values.ToArray();
			vertex_map.vertex_index = vertex_indices.ToArray();
			if (perpoly)
				vertex_map.polygon_index = polygon_indices.ToArray();
			return vertex_map;
		}
	};

	#endregion

	#region Points and polygons

	// points and polygons 
	public class Point
	{
		public Point(float x, float y, float z) { position_x = x; position_y = y; position_z = z; }

		public float position_x;
		public float position_y;
		public float position_z;

		public readonly List<uint>				Polygons	= new List<uint>();				// array of polygon indexes 
		public readonly List<VertexMapPoint>	VertexMaps	= new List<VertexMapPoint>();	// array of vmap references 
	};

	public class PolygonVertex 
	{
		public PolygonVertex(uint index) { Index = index; }
		public readonly uint Index;		// index into the point array 
		
		public float	normal_x;
		public float	normal_y;
		public float	normal_z;

		public readonly List<VertexMapPoint> VertexMaps		= new List<VertexMapPoint>();	// array of vmap references 
	};

	#region PolygonType (enum)
	public enum PolygonType : uint
	{
		ID_FACE = (uint)(((uint)'F') << 24 | ((uint)'A') << 16 | ((uint)'C') << 8 | ((uint)'E')),
		ID_CURV = (uint)(((uint)'C') << 24 | ((uint)'U') << 16 | ((uint)'R') << 8 | ((uint)'V')),
		ID_PTCH = (uint)(((uint)'P') << 24 | ((uint)'T') << 16 | ((uint)'C') << 8 | ((uint)'H')),
		ID_MBAL = (uint)(((uint)'M') << 24 | ((uint)'B') << 16 | ((uint)'A') << 8 | ((uint)'L')),
		ID_BONE = (uint)(((uint)'B') << 24 | ((uint)'O') << 16 | ((uint)'N') << 8 | ((uint)'E')),
	}
	#endregion

	public class Polygon 
	{
		public Polygon(PolygonType type, int flags = 0) { Type = type; Flags = flags; }
		public readonly int			Flags;
		public readonly PolygonType	Type;

		public Surface	SurfaceReference;	// ref
		
		public uint		surf_index;			// surface index
		public uint		part_index;			// part index 
		public uint		layer_index;		// layer index

		public int		SmoothingGroup;		// smoothing group 

		public float	normal_x;
		public float	normal_y;
		public float	normal_z;

		public readonly List<PolygonVertex> Vertices = new List<PolygonVertex>();	// array of vertex records 
	};

	#endregion

	#region Geometry Layers

	#region PolygonTags (enum)
	public enum PolygonTags : uint
	{
		ID_BNID = (uint)(((uint)'B') << 24 | ((uint)'N') << 16 | ((uint)'I') << 8 | ((uint)'D')),
		ID_SURF = (uint)(((uint)'S') << 24 | ((uint)'U') << 16 | ((uint)'R') << 8 | ((uint)'F')),
		ID_PART = (uint)(((uint)'P') << 24 | ((uint)'A') << 16 | ((uint)'R') << 8 | ((uint)'T')),
//		ID_SGMP = (uint)(((uint)'S') << 24 | ((uint)'G') << 16 | ((uint)'M') << 8 | ((uint)'P')), // wrong?
		ID_SMGP = (uint)(((uint)'S') << 24 | ((uint)'M') << 16 | ((uint)'G') << 8 | ((uint)'P')),
		ID_COLR = (uint)(((uint)'C') << 24 | ((uint)'O') << 16 | ((uint)'L') << 8 | ((uint)'R')),
		ID_LXGN = (uint)(((uint)'L') << 24 | ((uint)'X') << 16 | ((uint)'G') << 8 | ((uint)'N')),
	}
	#endregion

	// geometry layers 
	public class Layer 
	{
		public string			Name;
		public uint				Index;
		public int				Parent;
		public int				Flags;

		public float			pivot_x;
		public float			pivot_y;
		public float			pivot_z;

		public float			bbox_min_x;
		public float			bbox_min_y;
		public float			bbox_min_z;
		public float			bbox_max_x;
		public float			bbox_max_y;
		public float			bbox_max_z;

		public readonly List<Point>		Points		= new List<Point>();		// array of points 
		public readonly List<Polygon>	Polygons	= new List<Polygon>();		// array of polygons 
		public readonly List<VertexMap>	VertexMaps	= new List<VertexMap>();	// linked list of vmaps 


		internal void CalculateBoundingBox()
		{
			if (Points.Count == 0)
				return;

			if (!(bbox_min_x == 0.0f && bbox_min_y == 0.0f && bbox_min_z == 0.0f &&
				  bbox_max_x == 0.0f && bbox_max_y == 0.0f && bbox_max_z == 0.0f))
				return;

			bbox_min_x = bbox_min_y = bbox_min_z = float.PositiveInfinity;
			bbox_max_x = bbox_max_y = bbox_max_z = float.NegativeInfinity;
			for (var i = 0; i < Points.Count; i++)
			{
				if (bbox_min_x > Points[i].position_x) bbox_min_x = Points[i].position_x;
				if (bbox_max_x < Points[i].position_x) bbox_max_x = Points[i].position_x;

				if (bbox_min_y > Points[i].position_y) bbox_min_y = Points[i].position_y;
				if (bbox_max_y < Points[i].position_y) bbox_max_y = Points[i].position_y;

				if (bbox_min_z > Points[i].position_z) bbox_min_z = Points[i].position_z;
				if (bbox_max_z < Points[i].position_z) bbox_max_z = Points[i].position_z;
			}
		}
	};

	#endregion

	#region LightwaveObject
	public class LightwaveObject 
	{
		public readonly List<Layer>		Layers		= new List<Layer>();	// linked list of layers 
		public readonly List<Envelope>	Envelopes	= new List<Envelope>();	// linked list of envelopes 
		public readonly List<Clip>		Clips		= new List<Clip>();		// linked list of clips 
		public readonly List<Surface>	Surfaces	= new List<Surface>();	// linked list of surfaces 
		public readonly List<string>	Tags		= new List<string>();	// array of strings 


		#region CalculatePolygonNormals
		void CalculatePolygonNormals(List<Point> points, List<Polygon> polygons)
		{
			float p1_0, p1_1, p1_2;
			float p2_0, p2_1, p2_2;
			float pn_0, pn_1, pn_2;
			float v1_0, v1_1, v1_2;
			float v2_0, v2_1, v2_2;

			for (var i = 0; i < polygons.Count; i++)
			{
				var count = polygons[i].Vertices.Count;
				if (count < 3)
					continue;

				var vI0 = (int)polygons[i].Vertices[0].Index;
				var vI1 = (int)polygons[i].Vertices[1].Index;
				var vI2 = (int)polygons[i].Vertices[count - 1].Index;

				p1_0 = points[vI0].position_x;
				p2_0 = points[vI1].position_x;
				pn_0 = points[vI2].position_x;

				p1_1 = points[vI0].position_y;
				p2_1 = points[vI1].position_y;
				pn_1 = points[vI2].position_y;

				p1_2 = points[vI0].position_z;
				p2_2 = points[vI1].position_z;
				pn_2 = points[vI2].position_z;

				v1_0 = p2_0 - p1_0;
				v2_0 = pn_0 - p1_0;

				v1_1 = p2_1 - p1_1;
				v2_1 = pn_1 - p1_1;

				v1_2 = p2_2 - p1_2;
				v2_2 = pn_2 - p1_2;

				//cross( v1, v2, pol[ i ].norm );
				polygons[i].normal_x = v1_1 * v2_2 - v1_2 * v2_1;
				polygons[i].normal_y = v1_2 * v2_0 - v1_0 * v2_2;
				polygons[i].normal_z = v1_0 * v2_1 - v1_1 * v2_0;

				//normalize( pol[ i ].norm );
				var r = (float)Math.Sqrt(
						polygons[i].normal_x * polygons[i].normal_x +
						polygons[i].normal_y * polygons[i].normal_y +
						polygons[i].normal_z * polygons[i].normal_z
					);
				if (r > 0)
				{
					polygons[i].normal_x /= r;
					polygons[i].normal_y /= r;
					polygons[i].normal_z /= r;
				}
			}
		}
		#endregion

		#region ResolvePointPolygons
		void ResolvePointPolygons(List<Point> points, List<Polygon> polygons)
		{
			// fill in polygon array for each point 
			for (var polygonIndex = 0; polygonIndex < polygons.Count; polygonIndex++)
			{
				var polygon = polygons[polygonIndex];
				foreach(var polVert in polygon.Vertices)
					points[(int)polVert.Index]
						.Polygons
							.Add(
								(uint)polygonIndex
								);
			}
		}
		#endregion

		#region ResolvePolygonSurfaces
		void ResolvePolygonSurfaces(List<Polygon> polygonList, List<string> surfaceNames, List<Surface> surfaceList, uint layerIndex)
		{
			var surfaceLookup = new Surface[surfaceNames.Count];
			for (var i = 0; i < surfaceNames.Count; i++)
			{
				foreach (var surface in surfaceList)
				{
					if (string.Compare(surface.name, surfaceNames[i]) == 0)
					{
						surfaceLookup[i] = surface;
						break;
					}
				}
			}

			foreach (var polygon in polygonList)
			{
				polygon.layer_index = layerIndex;
				var surface_index = (int)polygon.surf_index;
				if (surface_index < 0 || 
					surface_index > surfaceLookup.Length)
					throw new Exception("Surface index out of bounds");	// TODO: create a proper exception class

				// make sure the surface exists, even if it's empty ...
				if (surfaceLookup[surface_index] == null)
				{
					var new_surface = new Surface();
					new_surface.name = surfaceNames[surface_index];
					surfaceList.Add(new_surface);
					surfaceLookup[surface_index] = new_surface;
				}

				polygon.SurfaceReference = surfaceLookup[surface_index];
			}
		}
		#endregion

		#region CalculateVertexNormals
		void CalculateVertexNormals(List<Point> points, List<Polygon> polygons)
		{
			for (var j = 0; j < polygons.Count; j++)
			{
				for (var n = 0; n < polygons[j].Vertices.Count; n++)
				{
					polygons[j].Vertices[n].normal_x = polygons[j].normal_x;
					polygons[j].Vertices[n].normal_y = polygons[j].normal_y;
					polygons[j].Vertices[n].normal_z = polygons[j].normal_z;

					if (polygons[j].SurfaceReference.smooth <= 0)
						continue;

					var p = (int)polygons[j].Vertices[n].Index;
					for (var g = 0; g < points[p].Polygons.Count; g++)
					{
						var h = (int)points[p].Polygons[g];
						if (h == j)
							continue;

						if (polygons[j].SmoothingGroup != polygons[h].SmoothingGroup)
							continue;

						//vecangle(polygon.pol[j].norm, polygon.pol[h].norm);
						var d = polygons[j].normal_x * polygons[h].normal_x +
								polygons[j].normal_y * polygons[h].normal_y +
								polygons[j].normal_z * polygons[h].normal_z;
						var a = Math.Acos(d);
						if (a > polygons[j].SurfaceReference.smooth)
							continue;

						polygons[j].Vertices[n].normal_x += polygons[h].normal_x;
						polygons[j].Vertices[n].normal_y += polygons[h].normal_y;
						polygons[j].Vertices[n].normal_z += polygons[h].normal_z;
					}

					//normalize(polygon.pol[j].v[n].norm);
					var z = polygons[j].Vertices[n].normal_x * polygons[j].Vertices[n].normal_x +
							polygons[j].Vertices[n].normal_y * polygons[j].Vertices[n].normal_y +
							polygons[j].Vertices[n].normal_z * polygons[j].Vertices[n].normal_z;
					
					var r = (float)Math.Sqrt(z);
					if (r > 0)
					{
						polygons[j].Vertices[n].normal_x /= r;
						polygons[j].Vertices[n].normal_y /= r;
						polygons[j].Vertices[n].normal_z /= r;
					}
				}
			}
		}
		#endregion

		#region ResolvePointVertexMaps
		void ResolvePointVertexMaps(List<Point> points, List<VertexMap> vmap)
		{
			// fill in vmap references for each mapped point 
			foreach (var vm in vmap)
			{
				if (vm.PerPoly)
					continue;
				
				for (var i = 0; i < vm.vertex_index.Length; i++)
				{
					var vertex_index = (int)vm.vertex_index[i];
					points[vertex_index]
						.VertexMaps
							.Add(
								new VertexMapPoint(vm, (uint)i)
							);
				}
			}
		}
		#endregion

		#region ResolvePolygonVertexMaps
		void ResolvePolygonVertexMaps(List<Polygon> polygons, List<VertexMap> vmap)
		{
			// fill in vmap references for each mapped point
			foreach (var vm in vmap)
			{
				if (!vm.PerPoly)
					continue;
				
				for (var i = 0; i < vm.vertex_index.Length; i++)
				{
					var	vertex_index		= vm.vertex_index[i];
					var polygon_index		= (int)vm.polygon_index[i];

					foreach (var polygon_vertex in polygons[ polygon_index ].Vertices)
					{
						if (vertex_index == polygon_vertex.Index)
						{
							polygon_vertex
								.VertexMaps
									.Add(
										new VertexMapPoint(vm, (uint)i)
									);
							break;
						}
					}
				}
			}
		}

		#endregion


		#region ReadTags
		static IEnumerable<string> ReadTags(ChunkReader reader)
		{
			while (reader.BytesLeft > 0)
				yield return reader.ReadString();
		}
		#endregion

		#region ReadPolygonTags
		static void ReadPolygonTags(ChunkReader reader, List<Polygon> polygons, uint polygon_offset, uint tag_offset)
		{
			var type = reader.ReadID<PolygonTags>();
			if (type != PolygonTags.ID_SURF &&
				type != PolygonTags.ID_PART &&
				type != PolygonTags.ID_SMGP)
			{
				if (type != PolygonTags.ID_COLR &&	// 'Sketch Color Name'?
					type != PolygonTags.ID_LXGN)	// unknown
					Console.WriteLine("Unknown polygon tag type " + reader.GetIDString((uint)type));
				return;
			}

			while (reader.BytesLeft > 0)
			{
				var polygonIndex	= (int)(reader.ReadVariableLengthIndex() + polygon_offset);
				var tagIndex		= (int)(reader.ReadVariableLengthIndex() + tag_offset);

				switch (type)
				{
					case PolygonTags.ID_SURF: polygons[polygonIndex].surf_index = (uint)tagIndex; break;
					case PolygonTags.ID_PART: polygons[polygonIndex].part_index = (uint)tagIndex; break;
					case PolygonTags.ID_SMGP: polygons[polygonIndex].SmoothingGroup = (int)tagIndex; break;
					
					default: 
						Console.WriteLine("Unknown polygon tag type " + reader.GetIDString((uint)type));				
						break;
				}
			}
		}
		#endregion
		
		#region ReadPolygons
		static IEnumerable<Polygon> ReadPolygons(ChunkReader reader, uint vertex_offset)
		{
			var type = reader.ReadID<PolygonType>();

			// fill in the new polygons 
			while (reader.BytesLeft > 0)
			{
				var vertex_count = reader.ReadUInt16();
				var flags = vertex_count & 0xFC00;
				vertex_count &= 0x03FF;

				var newPolygon = new Polygon(type, flags);
				for (var j = 0; j < vertex_count; j++)
					newPolygon.Vertices.Add(
							new PolygonVertex(reader.ReadVariableLengthIndex() + vertex_offset)
						);

				yield return newPolygon;
			}
		}
		#endregion

		#region ReadPolygons5
		// LWOB
		static IEnumerable<Polygon> ReadPolygons5(ChunkReader reader, uint vertex_offset)
		{
			// fill in the new polygons 
			while (reader.BytesLeft > 0)
			{
				var vertex_count = reader.ReadUInt16();
				var newPolygon = new Polygon(PolygonType.ID_FACE);
				for (var j = 0; j < vertex_count; j++)
					newPolygon.Vertices.Add(
							new PolygonVertex(reader.ReadUInt16() + vertex_offset)
						);

				var index = (int)reader.ReadSInt16();
				if (index < 0)
				{
					index = (int)-index;
					reader.ReadSInt16(); // skip 2 bytes - Q: why??
				}
				newPolygon.surf_index = (uint)(index - 1);
				yield return newPolygon;
			}
		}
		#endregion

		#region ReadPoints
		static IEnumerable<Point> ReadPoints(ChunkReader reader)
		{
			// assign position values 
			while (reader.BytesLeft > 0)
				yield return new Point(
						reader.ReadSingle(),
						reader.ReadSingle(),
						reader.ReadSingle()
					);
		}
		#endregion


		#region ChunkType (enum)
		enum ChunkType : uint
		{
			ID_LAYR = (uint)(((uint)'L') << 24 | ((uint)'A') << 16 | ((uint)'Y') << 8 | ((uint)'R')),
			ID_PNTS = (uint)(((uint)'P') << 24 | ((uint)'N') << 16 | ((uint)'T') << 8 | ((uint)'S')),
			ID_VMAP = (uint)(((uint)'V') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'P')),
			ID_VMAD = (uint)(((uint)'V') << 24 | ((uint)'M') << 16 | ((uint)'A') << 8 | ((uint)'D')),
			ID_VMPA = (uint)(((uint)'V') << 24 | ((uint)'M') << 16 | ((uint)'P') << 8 | ((uint)'A')),
			ID_POLS = (uint)(((uint)'P') << 24 | ((uint)'O') << 16 | ((uint)'L') << 8 | ((uint)'S')),
			ID_TAGS = (uint)(((uint)'T') << 24 | ((uint)'A') << 16 | ((uint)'G') << 8 | ((uint)'S')),
			ID_PTAG = (uint)(((uint)'P') << 24 | ((uint)'T') << 16 | ((uint)'A') << 8 | ((uint)'G')),
			ID_ENVL = (uint)(((uint)'E') << 24 | ((uint)'N') << 16 | ((uint)'V') << 8 | ((uint)'L')),
			ID_CLIP = (uint)(((uint)'C') << 24 | ((uint)'L') << 16 | ((uint)'I') << 8 | ((uint)'P')),
			ID_SURF = (uint)(((uint)'S') << 24 | ((uint)'U') << 16 | ((uint)'R') << 8 | ((uint)'F')),
			ID_BBOX = (uint)(((uint)'B') << 24 | ((uint)'B') << 16 | ((uint)'O') << 8 | ((uint)'X')),
			ID_DESC = (uint)(((uint)'D') << 24 | ((uint)'E') << 16 | ((uint)'S') << 8 | ((uint)'C')),
			ID_TEXT = (uint)(((uint)'T') << 24 | ((uint)'E') << 16 | ((uint)'X') << 8 | ((uint)'T')),
			ID_ICON = (uint)(((uint)'I') << 24 | ((uint)'C') << 16 | ((uint)'O') << 8 | ((uint)'N')),
			ID_SRFS = (uint)(((uint)'S') << 24 | ((uint)'R') << 16 | ((uint)'F') << 8 | ((uint)'S')),	// LWOB
		}
		#endregion


		#region ReadObject2

		static LightwaveObject ReadObject2(ChunkReader reader)
		{
			// allocate an object and a default layer 
			var newObject		= new LightwaveObject();
			var	currentLayer	= new Layer();
			newObject.Layers.Add(currentLayer);
			
			bool createdLayer	= false;
			uint pointOffset	= 0;
			uint polygonOffset	= 0;
			uint tagOffset		= 0;

			// process chunks as they're encountered 
			while (reader.BytesLeft > 0)
			{
				var id = reader.ReadID<ChunkType>();
				var cksize	= reader.ReadUInt32();				
				cksize += cksize & 1;

				using (var subchunkReader = reader.GetSubChunk(cksize))
				{
					switch (id)
					{
						case ChunkType.ID_LAYR:
						{
							if (createdLayer)
							{
								currentLayer = new Layer();
								newObject.Layers.Add(currentLayer);
							}

							createdLayer = true;
							currentLayer.Index		= subchunkReader.ReadUInt16();
							currentLayer.Flags		= subchunkReader.ReadUInt16();
							currentLayer.pivot_x	= subchunkReader.ReadSingle();
							currentLayer.pivot_y	= subchunkReader.ReadSingle();
							currentLayer.pivot_z	= subchunkReader.ReadSingle();
							currentLayer.Name		= subchunkReader.ReadString();
							if (subchunkReader.BytesLeft > 2)
								currentLayer.Parent = subchunkReader.ReadUInt16();
							break;
						}

						case ChunkType.ID_PTAG:
						{
							ReadPolygonTags(subchunkReader, currentLayer.Polygons, polygonOffset, tagOffset);
							break;
						}

						case ChunkType.ID_BBOX:
						{
							currentLayer.bbox_min_x = subchunkReader.ReadSingle();
							currentLayer.bbox_min_y = subchunkReader.ReadSingle();
							currentLayer.bbox_min_z = subchunkReader.ReadSingle();
							currentLayer.bbox_max_x = subchunkReader.ReadSingle();
							currentLayer.bbox_max_y = subchunkReader.ReadSingle();
							currentLayer.bbox_max_z = subchunkReader.ReadSingle();
							break;
						}

						case ChunkType.ID_PNTS:
						{
							pointOffset = (uint)currentLayer.Points.Count;
							currentLayer.Points.AddRange(
										ReadPoints(subchunkReader)							// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_POLS:
						{
							polygonOffset = (uint)currentLayer.Polygons.Count;
							currentLayer.Polygons.AddRange(
										ReadPolygons(subchunkReader, pointOffset)			// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_VMAP:
						{
							currentLayer.VertexMaps.Add(
										VertexMap.ReadVertexMap(subchunkReader, false)		// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_VMAD:
						{
							currentLayer.VertexMaps.Add(
										VertexMap.ReadVertexMap(subchunkReader, true)		// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_TAGS:
						{
							tagOffset = (uint)newObject.Tags.Count;
							newObject.Tags.AddRange(
										LightwaveObject.ReadTags(subchunkReader)			// throws exception on failure
									);	
							break;
						}

						case ChunkType.ID_ENVL:
						{
							newObject.Envelopes.Add(
										Envelope.ReadEnvelope(subchunkReader)				// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_CLIP:
						{
							newObject.Clips.Add( 
										Clip.ReadClip(subchunkReader)						// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_SURF:
						{
							newObject.Surfaces.Add(
										Surface.ReadSurface(subchunkReader)					// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_DESC: // Description Line - DESC { description-line[S0] }
						case ChunkType.ID_TEXT: // Commentary Text - TEXT { comment[S0] }
						case ChunkType.ID_ICON:	// Thumbnail Icon Image - ICON { encoding[U2], width[U2], data[U1] * }
						case ChunkType.ID_VMPA: // Vertex Map Parameter - VMPA { UV subdivision type[I4], sketch color[I4] }
							break;
						default:
							Console.WriteLine("Unknown chunk type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			if (newObject.Tags.Count == 0)
				throw new Exception("No tags found for this layer");	// TODO: create a proper exception class

			uint layer_index = 0;
			foreach (var layer in newObject.Layers)
			{
				layer.CalculateBoundingBox();
				newObject.CalculatePolygonNormals(layer.Points, layer.Polygons);
				newObject.ResolvePointPolygons(layer.Points, layer.Polygons);
				newObject.ResolvePolygonSurfaces(layer.Polygons, newObject.Tags, newObject.Surfaces, layer_index);
				newObject.CalculateVertexNormals(layer.Points, layer.Polygons);
				newObject.ResolvePointVertexMaps(layer.Points, layer.VertexMaps);
				newObject.ResolvePolygonVertexMaps(layer.Polygons, layer.VertexMaps);
				layer_index++;
			}

			return newObject;
		}

		#endregion

		#region ReadObject5

		// Returns the contents of a LWOB
		static LightwaveObject ReadObject5(ChunkReader reader)
		{
			// allocate an object and a default layer 
			var newObject		= new LightwaveObject();
			var currentLayer	= new Layer();
			newObject.Layers.Add(currentLayer);

			uint pointOffset	= 0;
			uint polygonOffset	= 0;
			uint tagOffset		= 0;
			
			// process chunks as they're encountered 
			while (reader.BytesLeft > 0)
			{
				var id		= reader.ReadID<ChunkType>();
				var cksize	= reader.ReadUInt32();
				cksize += cksize & 1;
				
				using (var subchunkReader = reader.GetSubChunk(cksize))
				{
					switch (id)
					{
						case ChunkType.ID_PNTS:
						{
							pointOffset = (uint)currentLayer.Points.Count;
							currentLayer.Points.AddRange(
										ReadPoints(subchunkReader)							// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_POLS:
						{
							polygonOffset = (uint)currentLayer.Polygons.Count;
							currentLayer.Polygons.AddRange(
										ReadPolygons5(subchunkReader, pointOffset)			// throws exception on failure
									);
							break;
						}

						case ChunkType.ID_SRFS:
						{
							tagOffset = (uint)newObject.Tags.Count;
							newObject.Tags.AddRange(
										LightwaveObject.ReadTags(subchunkReader)			// throws exception on failure
									);	
							break;
						}

						case ChunkType.ID_SURF:
						{
							newObject.Surfaces.Add(
										Surface.ReadSurface5(subchunkReader, newObject)		// throws exception on failure
									);
							break;
						}

						default:
							Console.WriteLine("Unknown chunk type " + reader.GetIDString((uint)id));
							break;
					}
				}
			}

			if (newObject.Tags.Count == 0)
				throw new Exception("No tags found for this layer");	// TODO: create a proper exception class

			uint layer_index = 0;
			foreach (var layer in newObject.Layers)
			{
				layer.CalculateBoundingBox();
				newObject.CalculatePolygonNormals(layer.Points, layer.Polygons);
				newObject.ResolvePointPolygons(layer.Points, layer.Polygons);
				newObject.ResolvePolygonSurfaces(layer.Polygons, newObject.Tags, newObject.Surfaces, layer_index);
				newObject.CalculateVertexNormals(layer.Points, layer.Polygons);
				layer_index++;
			}

			return newObject;
		}

		#endregion


		#region HeaderID (enum)
		enum HeaderID : uint
		{
			ID_FORM = (uint)(((uint)'F') << 24 | ((uint)'O') << 16 | ((uint)'R') << 8 | ((uint)'M')),
		}
		#endregion

		#region ObjectType (enum)
		enum ObjectType : uint
		{
			ID_LWO2 = (uint)(((uint)'L') << 24 | ((uint)'W') << 16 | ((uint)'O') << 8 | ((uint)'2')),
			ID_LWOB = (uint)(((uint)'L') << 24 | ((uint)'W') << 16 | ((uint)'O') << 8 | ((uint)'B')),
		}
		#endregion


		public static LightwaveObject LoadObject(string filename)
		{
			// open the file 
			var contents = File.ReadAllBytes(filename);
			using (var reader = new ChunkReader(contents))
			{
				// read the first 12 bytes 
				var id			= reader.ReadID<HeaderID>();
				var formsize	= reader.ReadUInt32();
				var type		= reader.ReadID<ObjectType>();
				

				if (id != HeaderID.ID_FORM) // is this a LW object? 
					return null;

				using (var formReader = reader.GetSubChunk(formsize))
				{
					switch (type)
					{
						case ObjectType.ID_LWO2: return ReadObject2(formReader);
						case ObjectType.ID_LWOB: return ReadObject5(formReader);
						default:
							Console.WriteLine("Unknown object type " + reader.GetIDString((uint)type));
							return null;
					}
				}
			}
		}
	}

	#endregion
}
