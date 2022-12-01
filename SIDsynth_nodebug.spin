CON''### HEADER ###
{{
  #############################################################
  #                                                           #
  # SID Polysynth v0.5 (15/08/2011)                           #
  # by Alessandro De Luca                                     #
  #                                                           #
  # - using SIDcog 1.00 by Johannes Ahlebrand                 #
  # - using StereoSpatializer 1.0 by Chip Gracey              #
  # - using FullDuplexSerial4FC 1.1 by Tim Moore              #
  #                                                           #
  #############################################################
}}

CON''### CONFIG OPTIONS ###
'=============================================================================
  _CLKMODE = xtal1 + pll16x
  _XINFREQ = 5_000_000
'=============================================================================
'  DEBUG_ENABLE = false
'  DEBUG_ENABLE = true
'=============================================================================
{
  MIDI_PIN      = 0             'pin is TX, pin+1 is RX
  AUDR_PIN      = 10            'Audio Right output pin
  AUDL_PIN      = 11            'Audio Left output pin
}
  MIDI_PIN      = 25            'pin is TX, pin+1 is RX
  AUDR_PIN      = 24            'Audio Right output pin
  AUDL_PIN      = 27            'Audio Left output pin
'-----------------------------------------------------------------------------
  MIDI_BRATE    = 31250         'true MIDI port
'  MIDI_BRATE    = 38400         'for use with Roland serial driver
'-----------------------------------------------------------------------------
'  MIDI_MODE     = 0             'TTL serial or other translator IC
  MIDI_MODE     = 4             '!! MIDI optoisolator needs inverted levels !!
'=============================================================================
  DEBUG_PIN     = 30            'pin is TX, pin+1 is RX
'-----------------------------------------------------------------------------
  DEBUG_BRATE   = 115200        'fastest possible to minimize delays
'  DEBUG_BRATE   = 230400        'ok for FDSerial, pcFDSerial4FC cannot cope!
'-----------------------------------------------------------------------------
  DEBUG_MODE    = 0             'normal non-inverted mode for debug port
'=============================================================================
  RETRIG_ON_SUS_CHANGE = 1
  LFO_DEPTH_BEHAVIOUR  = 1
'-----------------------------------------------------------------------------
  MOD_SPEED_LIMIT = 1
  MOD_DISP_DELTA  = 0
'-----------------------------------------------------------------------------

'#define _DEBUG_

#define MOD_ENABLE
'#define MOD_DISABLE

  DEBUG_CC = false

#ifdef _DEBUG_
  DEBUG_ENABLE = true
#else
  DEBUG_ENABLE = false
#endif

'=============================================================================

CON''### NOTES ###
{{
  Maximizing use of two five position switches
  for VCO waveform selection.

  OSC1 WAVE knob, left to right: TRI, SAW, SQR, PWM, RND
  OSC2 WAVE knob, left to right: TRI, SAW, SQR, EFX, SUB

  WAVE knobs   OSC1 settings           OSC2 settings
  ==========   =====================   =====================
  OSC1  OSC2   effWF PW  Int Det Flg   effWF PW  Int Det Flg
  ==========   =====================   =====================
  TRI    x     TRI  0800  0   0   0     x     x   x   x   x
  SAW    x     SAW  0800  0   0   0     x     x   x   x   x
  SQR    x     PUL  0800  0   0   0     x     x   x   x   x
  PWM    x     PUL   (1)  0   0   0     x     x   x   x   x
  RND    x     RND  0800  0   0   0     x     x   x   x   x
  ==========   =====================   =====================
   x    TRI     x     x   x   x   x    TRI  0800  0   0   0
   x    SAW     x     x   x   x   x    SAW  0800  0   0   0
   x    SQR     x     x   x   x   x    PUL  0800  0   0   0
  ----------   ---------------------   ---------------------
  TRI   EFX    TRI  0800  x   x  RNG   TRI  0800  0   0  RNG
  SAW   EFX    SAW  0800  x   x  SYN   SAW  0800  0   0  SYN
  SQR   EFX    PUL  02AB  x   x   0    PUL  0200  0   0   0
  PWM   EFX    SAW  0800  x   x   0    (2)  0800  0   0   0
  RND   EFX    RND  0800  x   x   0    WT0  0800  0   0   0
  ----------   ---------------------   ---------------------
  x     SUB     x     x   x   x   0    TRI  0800 -12  0   0
  ==========   =====================   =====================

(1) OSC1 PW is velocity sensitive:

  PW1 := 2560 - (vel << 4) *** CHECKME: formula changed!?! ***

  velocity   PWreg   Duty
  --------   -----   ----
    1        2544    62,1%
   ..         ..      ..
   32        2048    50.0%
   ..         ..      ..
   64        1536    37.5%
   ..         ..      ..
   96        1024    25.0%
   ..         ..      ..
  127         528    12.9%


(2) OSC2 has variable triangle slope (simulated):

  velocity   slope            waveform
  --------   -----            --------
    1..27    SL= 64 (50.0%)   TRI ( 16)
   28..52    SL= 80 (62.5%)   WT1 ( 80)
   53..77    SL= 96 (75.0%)   WT2 ( 96)
   78..102   SL=112 (87.5%)   WT3 (112)
  103..127   SL=128 (100 %)   SAW ( 32)

  NOTE: with [50.0, 62.5, 75.0, 87.5, 100] the gap
        between 87.5 and 100 is too noticeable.
        Now using [50, 70, 85, 95, 100] instead!

(3)
  Sine Wave WT0 ( 48) used for:
    LFO     - POLY or default mode? (mod is 8 bit anyway :-)
    RND+EFX - noise combined with sine (all vs single tone!)
    SUB     - *NOT!* -> using triangle for better resolution!
}}
CON''### CONSTANTS ###
'=============================================================================
  BUFFER_SIZE    = $0E00 '$1000 'TODO: hack SSP for dynamic buffer size?!?
  NUM_VOICES     = 4
  MAX_VOICE      = NUM_VOICES - 1
'=============================================================================
  MSG_NOTEOFF    = $80
  MSG_NOTEON     = $90
  MSG_POLYPRESS  = $A0
  MSG_CTLCHANGE  = $B0
  MSG_PRGCHANGE  = $C0
  MSG_CHANPRESS  = $D0
  MSG_PITCHBEND  = $E0
  MSG_SYSTEM     = $F0
'-----------------------------------------------------------------------------
  CC_MODWHEEL    = $01 '  1
  CC_DATAENT_LSB = $06 '  6
  CC_VOLUME      = $07 '  7
  CC_PANPOT      = $0A ' 10
  CC_EXPRESSION  = $0B ' 11
  CC_DATAENT_MSB = $26 ' 38
  CC_SUSPEDAL    = $40 ' 64
  CC_NRPN_LSB    = $62 ' 98
  CC_NRPN_MSB    = $63 ' 99
  CC_RPN_LSB     = $64 '100
  CC_RPN_MSB     = $65 '101
  CC_ALLNOTESOFF = $7B '123
'=============================================================================
' PRIMARY CCs (values are EMU X-Board 61 defaults, change as necessary!)
'-----------------------------------------------------------------------------
  CC_DO1_WAVE    = $15 ' 21
  CC_DO2_WAVE    = $16 ' 22
  CC_DO2_INTVAL  = $17 ' 23
  CC_DO2_DETUNE  = $18 ' 24
'-----------------------------------------------------------------------------
  CC_VCF_ATTACK  = $19 ' 25     \  Those controls affect ALL 3 envelopes
  CC_VCF_DECAY   = $1A ' 26     |_ and not only EG3!!! After setting master
  CC_VCF_SUSTAIN = $1B ' 27     |  value here, use DO1_x/DO2_x controls to
  CC_VCF_RELEASE = $1C ' 28     /  set variations on EG1/EG2 times!
'-----------------------------------------------------------------------------
  CC_LFO_WAVE    = $46 ' 70
  CC_LFO_RATE    = $47 ' 71
  CC_LFO_DELAY   = $48 ' 72
  CC_VCF_ENVAMT  = $49 ' 73
'-----------------------------------------------------------------------------
  CC_VCF_CUTOFF  = $5B ' 91
  CC_VCF_RESON   = $5D ' 93
  CC_VCA_CTLBAL  = $52 ' 82
  CC_VCA_OSCBAL  = $53 ' 83
'=============================================================================
' EXTENDED CCs (not yet implemented)
'-----------------------------------------------------------------------------
  CC_DO1_ATTACK  = $35 ' 53 grp1+$20
  CC_DO1_DECAY   = $36 ' 54 grp1+$20
  CC_DO1_SUSTAIN = $37 ' 55 grp1+$20
  CC_DO1_RELEASE = $38 ' 56 grp1+$20
'-----------------------------------------------------------------------------
  CC_DO2_ATTACK  = $39 ' 57 grp2+$20
  CC_DO2_DECAY   = $3A ' 58 grp2+$20
  CC_DO2_SUSTAIN = $3B ' 59 grp2+$20
  CC_DO2_RELEASE = $3C ' 60 grp2+$20
'-----------------------------------------------------------------------------
  CC_VCO_V2EGAT  = $56 ' 86 grp3+$10   Vel to EG Attack Time
  CC_VCO_K2EGDT  = $57 ' 87 grp3+$10   Key to EG Decay/Release Time
  CC_DO1_INTVAL  = $58 ' 88 grp3+$10   Master Transpose (?)
  CC_DO1_DETUNE  = $59 ' 89 grp3+$10   Master Tuning    (?)
'-----------------------------------------------------------------------------
  CC_VCF_KEYFLW  = $29 ' 41 grp2+$10   VCF keyfollow scaling
  CC_VCF_L2EG    = $2A ' 42 grp2+$10
  CC_MIX_CHORUS  = $2B ' 43 grp2+$10   Chorus Level    FIXME: use GM std #
  CC_MIX_REVERB  = $2C ' 44 grp2+$10   Reverb Level    FIXME: use GM std #
'-----------------------------------------------------------------------------
'=============================================================================
  'standard waves
  WF_OSCOFF      = $00
  WF_TRIANGLE    = $10
  WF_SAWTOOTH    = $20
  WF_WTABLE0     = $30
  WF_PULSE       = $40
  WF_WTABLE1     = $50
  WF_WTABLE2     = $60
  WF_WTABLE3     = $70
  WF_RANDOM      = $80
'-----------------------------------------------------------------------------
  'special waves
  WF_SQUARE      = $40 'alias for pulse, indicate PW=50%
  WF_VELPWM      = $90 'velocity sensitive PW
  WF_EFFECTS     = $A0 'RING, SYNC, and more weird stuff
  WF_SUBOSC      = $B0 'TRI one octave down
'-----------------------------------------------------------------------------
  'lfo waves
  WF_POLYTRIG    = $C0 'sync to key
  WF_RAMPDOWN    = $D0 'SAW wave inverted
  WF_SINE        = $E0 'ROM table lookup?
  WF_RESERVED    = $F0 '???
  WF_RAMPUP      = $20 'alias for sawtooth
  WF_SAMNHOLD    = $80 'alias for random
'=============================================================================
  PW_SQUARE      = $0800
'-----------------------------------------------------------------------------
  VOX_ST_FREE    = 0
  VOX_ST_GATE    = 1
  VOX_ST_PLAY    = 2
  VOX_ST_GOFF    = 3
  VOX_ST_RLSE    = 4
  VOX_ST_SHUT    = 5
'-----------------------------------------------------------------------------
  VF_HELD        = 1
  MODBRATE       = 64
'=============================================================================
  LFO_ST_IDLE    = 0
  LFO_ST_WAIT    = 1
  LFO_ST_RAMP    = 2
  LFO_ST_HOLD    = 3
  LFO_ST_FALL    = 4
  LFO_RAMP_MAX   = $01_00_00_00
'=============================================================================

OBJ''### MODULES ###
                                   'COG 0 running SPIN init/main
  serial : "FullDuplexSerial"      '<- shut off during init, replaced by SSP (COG1)
'  stereo : "StereoDuty_mod"        '<- just a test gone bad :-D
  stereo : "StereoSpatializer"     'COG 1 running mixer/FX
  sid[NUM_VOICES] : "SIDcog_mod21" 'COG 2..5 running Tone Generators
  uart   : "pcFullDuplexSerial4FC" 'COG 6 running multiple serial driver
                                   'COG 7 running modulation manager thread
OBJ''### INCLUDES ###

  p_ : "Patch"
  c_ : "Channel"
  v_ : "Voice"

VAR''### GLOBAL VARIABLES ###
  '--- subtask stack & chip pointers ---
  long modstack[64]
  long regp[4] 'pointers to SID registers

  '--- tonegen data structures
  byte str_pch[p_#PATCH_SIZE]
  byte str_chn[c_#CHANNEL_SIZE]
  byte str_vox[v_#VOICE_SIZE * NUM_VOICES]

  '--- spatializer structure ---
  word s_input[4]               '  0
  word s_angle[4]               '  8
  word s_depth[4]               ' 16
  word s_knobs, s_RSVD1         ' 24
  long s_state[4], s_rnd        ' 28
  long s_RESV1[4]               ' 48
  long s_buffer[BUFFER_SIZE]    ' 64
  '- SIZE = 64 (48) + BUFFER_SIZE

  '--- misc globals ---
  long g_vcfadj, g_tuning, g_rrcount,  g_altknobs
  long g_ramptk, g_currpoly, g_dticks, _g_cnt0
  long g_psus[3], g_pwf[3]
  long modblink, modcount, g_delta,  g_delta_max

  long _pp, _cp
  long voxp[4]

  long vp_sort[4]

PUB Init | i
  Delay(3000) 'allow time to switch on debug terminal
'  stereo.start(@samPtr, 10, 11)
'  waitcnt(cnt + clkfreq>>2)
  serial.start(DEBUG_PIN+1, DEBUG_PIN, DEBUG_MODE, DEBUG_BRATE)
  Delay(250)
  serial.str(string(27,"[2J",27,"[H"))
  serial.str(string("SID PolySynth v0.5",13,10))
  serial.str(string("by Alessandro De Luca",13,10))
  serial.str(string("using SIDcog 1.00 by Johannes Ahlebrand",13,10))
  serial.str(string("using StereoSpatializer 1.0 by Chip Gracey",13,10))
  serial.str(string("using FullDuplexSerial4FC 1.1 by Tim Moore",13,10))
  Delay(250)
{
  _cp := p_.GetPtr(0)
  _pp := c_.GetPtr(0)
  repeat i from 0 to MAX_VOICE
    voxp[i] := v_.GetPtr(i)
}
  _pp := @str_pch
  _cp := @str_chn
  repeat i from 0 to MAX_VOICE
    voxp[i] := @str_vox + (i * v_#VOICE_SIZE)
{
    serial.tx("!")
    serial.dec(i)
    serial.tx(32)
    serial.hex(@vox, 6)
    serial.tx(32)
    serial.hex(voxp[i], 6)
    serial.tx(13)
    serial.tx(10)
}
'  OUTA[23..16] := %00000000
'  DIRA[23..16] := %11111111

  g_ramptk := clkfreq / 256_000     'ticks in 1/256 of a millisecond
  g_dticks := clkfreq / 160 '100'200         'modulation rate, 500000 @ 80MHz
  g_delta_max := 0

  serial.str(string("--------- STARTING COGS ---------",13,10))

  repeat i from 0 to MAX_VOICE
'    regp[i] := sid[i].start( 16+(i*2), 17*(i*2) )  'start a sidcog
    regp[i] := sid[i].start(4+i, -1)   'start a sidcog
'    samPtr[i] := regp[i] + 32
    serial.str(string("  COG"))
    serial.dec(2+i)
    serial.str(string(": SIDcog #"))
    serial.dec(i)
    serial.str(string(" started - SID output $"))
    serial.hex(regp[i], 4)
    serial.str(string(" - SSP input $"))
'    s_input[i] := sid[i].getSamplePtr  'set spatializer inputs
    s_input[i] := regp[i] + 32 '28  'set spatializer inputs
    serial.hex(s_input[i], 4)
    serial.str(string(" ",13,10))
    s_angle[i] := i * $5555               'spread out channels evenly across sound stage
    s_depth[i] := 256+(((i>>1)^(i&1))<<9)
'    s_depth[i] := 0
'  s_knobs := %000_110_110_000             'set spatializer echoes
  s_knobs := %001_010_010_001             'set spatializer echoes
'  s_knobs := %001_101_101_001             'set spatializer echoes

  Delay(250)
  serial.stop 'free COG1
  Delay(125)
  stereo.start(@s_input, @s_buffer, BUFFER_SIZE, AUDL_PIN, -1, AUDR_PIN, -1) 'start spatializer
  Delay(125)

'  serial.start(DEBUGp_N+1, DEBUGp_N, 0, DEBUG_BRATE)
  uart.Init
  uart.AddPort(0, MIDI_PIN+1, MIDI_PIN+0,uart#PINNOTUSED,uart#PINNOTUSED,uart#DEFAULTTHRESHOLD,MIDI_MODE, MIDI_BRATE )
  uart.AddPort(1,DEBUG_PIN+1,DEBUG_PIN+0,uart#PINNOTUSED,uart#PINNOTUSED,uart#DEFAULTTHRESHOLD,DEBUG_MODE,DEBUG_BRATE)
  uart.Start

  Delay(250)
  uart.str(1,string("  COG1: Stereo Spatializer started",13,10))

  Delay(250)
'  midiio.start(MIDIp_N+1, MIDIp_N, 0, MIDI_RATE)  'USB to ttl and Roland Serial MIDI driver
  uart.str(1,string("  COG6: MIDI port started at pins "))
  uart.dec(1,MIDI_PIN)
  uart.tx(1,",")
  uart.dec(1,MIDI_PIN+1)
  uart.tx(1,13)
  uart.tx(1,10)
  Delay(250)
#ifdef MOD_ENABLE
  cognew(ModTask, @modstack)
  uart.str(1,string("  COG7: modulation thread started",13,10))
#endif
  uart.str(1,string("---- INITIALIZATION COMPLETE ----",13,10))
  Delay(1000)
  uart.rxflush(0)
  uart.rxflush(1)

  g_currpoly := 0

  Main

PUB Main | pst, cmd, mch, db0, db1, db2, mcount, mblink
  mcount := MODBRATE
  mblink := 0
{
  repeat i from 0 to 255
    uart.tx(0,i)
    waitcnt(cnt+clkfreq>>4)
}
  g_altknobs := false
  GMReset
  pst := MSG_NOTEOFF
  repeat
{
    tmp := uart.rxcheck(1)
    if tmp <> -1
      uart.str(1,string("*** ["))
      uart.dec(1,tmp)
      uart.str(1,string("]",13,10))
    if tmp == 32
      'DumpAll
}
    db0 := uart.rxcheck(0)
    if db0 <> -1
      uart.tx(0,db0)
      if db0 < MSG_NOTEOFF
        'running status
        'get status byte from last received
        db1 := db0
        db0 := pst
'        PrintMsg2(string("+RUNSTAT: "), db0, db1)
      else
        'normal message
        'unless it's a system msg save status and get first data byte
        if db0 < MSG_SYSTEM
          pst := db0
          db1 := uart.rx(0)
'        PrintMsg2(string("+FULLMSG: "), db0, db1)
      cmd := db0 & $F0
      mch := db0 & $0F
      if (cmd <> MSG_PRGCHANGE) and (cmd <> MSG_CHANPRESS) and (cmd <> MSG_SYSTEM)
        db2 := uart.rx(0)

      case cmd
        MSG_NOTEOFF:
          NoteOff(mch, db1, db2)
        MSG_NOTEON:
          NoteOn( mch, db1, db2)
        MSG_POLYPRESS:
          PolyPress(mch, db1, db2)
        MSG_CTLCHANGE:
          CtlChange(mch, db1, db2)
        MSG_PRGCHANGE:
          PrgChange(mch, db1)
        MSG_CHANPRESS:
          ChanPress(mch, db1)
        MSG_PITCHBEND:
          PitchBend(mch, db1, db2)
        MSG_SYSTEM:
          System(db0)
        other:
          PrintMsg1(string("WARNING: running status leaked!"), db0)
{
    if c_modwheel > 0
      repeat i from 0 to MAX_VOICE
        uart.dec(1,i)
        uart.str(1,string(" O3="))
        uart.dec(1,v_det[i])
        uart.str(1,string(" E3="))
        uart.dec(1,v_cfm[i])
        uart.str(1,string("   "))
      uart.tx(1,13)
      uart.tx(1,10)
}
    if (--mcount == 0)
      mcount := MODBRATE
      if MOD_DISP_DELTA
        if g_delta > g_delta_max
          g_delta_max := g_delta
        uart.str(1,string("delta="))
        uart.dec(1,g_delta)
        uart.str(1,string("  max="))
        uart.dec(1,g_delta_max)
        uart.str(1,string(13,10))
      if g_currpoly < 0
        uart.str(1,string("*******************************",13,10))
        uart.str(1,string("*** NEGATIVE POLY COUNT !!! ***",13,10))
        uart.str(1,string("*******************************",13,10))
        CtlChange(0, CC_ALLNOTESOFF, 0)

CON''### TIMING, DEBUG, MISC ###
PUB Delay(ms) | t_dly
'
' Wait for a specified number of milliseconds.
'
  t_dly := (clkfreq / 1000) * ms
  waitcnt(cnt + t_dly)
{
PRI DumpVoxPtr(val) | i
  uart.tx(1,"!")
  uart.hex(1,val, 6)
  uart.tx(1,32)
  uart.hex(1,@str_vox, 6)
  uart.tx(1,32)
  uart.tx(1,":")
  repeat i from 0 to MAX_VOICE
    uart.tx(1,32)
    uart.hex(1,voxp[i], 5)
  PrintCRLF

PRI DumpCurrPoly
  uart.str(1,string("  Poly = "))
  uart.dec(1,g_currpoly)
  PrintCRLF

PRI DumpVMask | i, _vp, tval, tptr
  uart.str(1,string(" GPoly="))
  uart.dec(1, g_currpoly)
  uart.str(1,string(" VMask="))
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
'    uart.hex(1,_vp+v_#chn,4)
'    uart.tx(1,":")
    tval := byte[_vp][v_#state]
    tptr := @VSNames
    if tval > 6
      tptr += 48
    else
      tptr += tval << 3
    uart.str(1,tptr)
    uart.tx(1,"/")
    tval := byte[_vp][v_#lfo_state]
    tptr := @MSNames
    if tval > 6
      tptr += 48
    else
      tptr += tval << 3
    uart.str(1,tptr)
    uart.tx(1," ")
  PrintCRLF
}
PRI GetElapsedTicks | t_cnt1, t_delta
'
' Get number of ticks elapsed since previous call.
'
  t_cnt1 := cnt & $3FFF_FFFF    'avoid signed range, plus one more bit
  t_delta := t_cnt1 - _g_cnt0   'delta time from previous call
  if (t_delta < 0)
    t_delta += $4000_0000       'handle rollover of counter
  _g_cnt0 := t_cnt1             'save current value for next turn
  g_delta := t_delta ''export delta for debugging
  return t_delta

PRI PrintCRLF
  uart.tx(1,13)
  uart.tx(1,10)

PRI PrintMsg0(str)
  if DEBUG_ENABLE
    uart.str(1,str)
    PrintCRLF

PRI PrintMsg1(str, val)
  if DEBUG_ENABLE
    uart.str(1,str)
    uart.tx(1,32)
    uart.hex(1,val,2)
    PrintCRLF
{
PRI PrintMsg2(str, mch, db1)
  if DEBUG_ENABLE
    uart.str(1,str)
    uart.tx(1,32)
    uart.hex(1,mch,2)
    uart.tx(1,32)
    uart.hex(1,db1,2)
    PrintCRLF

PRI PrintMsg3(str, mch, db1, db2)
  if DEBUG_ENABLE
    uart.str(1,str)
    uart.tx(1,32)
    uart.hex(1,mch,2)
    uart.tx(1,32)
    uart.hex(1,db1,2)
    uart.tx(1,32)
    uart.hex(1,db2,2)
    PrintCRLF

PRI PrintMsg5(str, mch, a, b, c, d)
  if DEBUG_ENABLE
    uart.str(1,str)
    uart.tx(1,32)
    uart.hex(1,mch,2)
    uart.tx(1,32)
    uart.hex(1,a,2)
    uart.tx(1,32)
    uart.hex(1,b,2)
    uart.tx(1,32)
    uart.hex(1,c,2)
    uart.tx(1,32)
    uart.hex(1,d,2)
    PrintCRLF

PRI PrintCC(str, val)
  if DEBUG_CC
    PrintMsg1(str, val)

PRI PrintWName(cl, wf) | n, tptr
  n := wf >> 4
  tptr := @WNames0 + (((cl << 6) | (wf >> 2)) & $FC)
  uart.str(1, tptr)

PRI DumpRegs(n) | i, j, ptr
  if DEBUG_ENABLE
    uart.str(1,string("Chip #"))
    uart.dec(1,n)
    uart.str(1,string("  FL FH PL PH CR AD SR --",13,10))
    ptr := regp[n]
    repeat j from 0 to 2
      uart.str(1,string("  OSC"))
      uart.dec(1,j+1)
      uart.tx(1,32)
      uart.tx(1,":")
      repeat i from 0 to 7
        uart.tx(1,32)
        uart.hex(1,byte[ptr+(j<<3)+i], 2)
      PrintCRLF
    uart.str(1,string("         FL FH RS MV PX PY O3 E3",13,10))
    uart.str(1,string("  CTRL :"))
    repeat i from 0 to 7
      uart.tx(1,32)
      uart.hex(1,byte[ptr+24+i], 2)
    PrintCRLF
    uart.str(1,string("         DEPTH ANGLE",13,10))
    uart.str(1,string("  STSP : "))
    uart.hex(1,s_depth[n], 4)
    uart.tx(1,32)
    uart.tx(1,32)
    uart.hex(1,s_angle[n], 4)
    PrintCRLF

PRI DumpAll | i
  if DEBUG_ENABLE
    repeat i from 0 to MAX_VOICE
      DumpRegs(i)

PRI _HexRow(a,b,c,d,rem)
  uart.str(1,string("  byte      $"))
  uart.hex(1,a,2)
  uart.str(1,string(",$"))
  uart.hex(1,b,2)
  uart.str(1,string(",$"))
  uart.hex(1,c,2)
  uart.str(1,string(",$"))
  uart.hex(1,d,2)
  uart.str(1,string("     '"))
  uart.str(1,rem)
  uart.str(1,string(13,10))

PUB DumpPatch | tndx
  tndx := 0
  '--- HEADER ---
  uart.str(1,string(13,10,"--- cut here ---------------------------------------------",13,10))
  uart.str(1,string("_PatchX",13,10))
  uart.str(1,string("  '-- NAME --",13,10))
  uart.str(1,string("_begX",13,10))
  uart.str(1,string("  byte      ",34,"00X:ChangeMe",34,"      'p_name[16]",13,10))
  uart.str(1,string("_endX",13,10))
  uart.str(1,string("  byte      0[16 - (@_endX - @_begX)]",13,10))
  tndx += 16
  '-- OSC1 --
  uart.str(1,string("  '-- OSC1 --",13,10))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("VCO: p_do1_wf,     p_do1_pw,     p_do1_intval,  p_do1_detune"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("MOD: x_do1_lfoamt, x_do1_lfo2pw, p_do1_cat2pw,  p_do1_egttrk"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("ENV: p_do1_attack, p_do1_decay,  p_do1_sustain, p_do1_release"))
  '-- EFX/VCA --
  uart.str(1,string("  '-- EFX/VCA --",13,10))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("VCA: x_chorus,     x_reverb,     p_vca_ctlbal,  p_vca_oscbal"))
  '-- OSC2 --
  uart.str(1,string("  '-- OSC2 --",13,10))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("VCO: p_do2_wf,     p_do2_pw,     p_do2_intval,  p_do2_detune"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("MOD: x_do2_lfoamt, x_do2_lfo2pw, p_do2_cat2pw,  p_do2_egttrk"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("ENV: p_do2_attack, p_do2_decay,  p_do2_sustain, p_do2_release"))
  '-- LFO --
  uart.str(1,string("  '-- LFO --",13,10))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("LFO: p_lfo_wf,     p_lfo_rate,   p_lfo_delay,   p_lfo_mdest"))
  '-- FILT --
  uart.str(1,string("  '-- FILT --",13,10))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("VCF: p_vcf_cutoff, p_vcf_reson,  p_vcf_keyflw,  x_vcf_omask"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("MOD: x_vcf_lfoamt, p_vcf_envamt, x_vcf_velsns,  p_vcf_egttrk"))
  _HexRow(byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++],byte[_pp][tndx++], {
    } string("ENV: p_vcf_attack, p_vcf_decay,  p_vcf_sustain, p_vcf_release"))
  '-- RESV --
  uart.str(1,string("  '-- RESV --",13,10))
  _HexRow($DE,$AD,$BE,$EF,string("RSV: p_RESERVED1,  p_RESERVED2,  p_RESERVED3,   p_RESERVED4"))
  tndx += 4
  '--- FOOTER ---
  uart.str(1,string("--- cut here ---------------------------------------------",13,10,13,10))
  uart.str(1,string("("))
  uart.dec(1,tndx)
  uart.str(1,string(" bytes)",13,10))

PRI VoiceToLeds | i, _vp
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if byte[_vp][v_#state] == VOX_ST_PLAY
      OUTA[20+i] := 1
    else
      OUTA[20+i] := 0

PRI ub2si(val) | tval
'unsigned byte to signed int
  tval := val & 255
  if tval & 128
    tval := tval - 256
  return tval

PRI si2ub(val) | tval
'signed int to unsigned byte
  if val < -128
    tval := -128
  elseif val > 127
    tval := 127
  else
    tval := val
  if tval < 0
    tval := tval + 256
  return tval
}
PRI GetSymmVal(lsb, msb) | rval
'
' Make a bipolar control "symmetric" (e.g. Pitch Bender)
' by using the LSB value before we throw it away.
'
  if lsb < 64
    rval := msb - 64        ' -64..0..+63
  else
    rval := msb - 63        ' -63..0..+64
  return rval

CON''### SID HELPERS ###
PRI SetWaveform1(wf, pw) | i
  repeat i from 0 to MAX_VOICE
    sid[i].SetWaveform(0, wf & $F0)
    sid[i].setPulseWidth(0, pw)

PRI SetWaveform2(wf, pw) | i
  repeat i from 0 to MAX_VOICE
    sid[i].SetWaveform(1, wf & $F0)
    sid[i].setPulseWidth(1, pw)

PRI SetWaveform3(wf) | i
  repeat i from 0 to MAX_VOICE
    sid[i].SetWaveform(2, wf & $F0)
    sid[i].setPulseWidth(2, PW_SQUARE)

PRI SetDuty1(duty) | i, epw

'  if duty => 64
'    epw := 2048 - ((duty - 64) << 5)
  epw := 2560 - (duty<<4)
'  epw := 2048 - (duty<<4)
  repeat i from 0 to MAX_VOICE
    sid[i].setPulseWidth(0, epw)

PRI SetDuty2(duty) | i, ewf
  ewf := STable[duty>>3]
  repeat i from 0 to MAX_VOICE
    sid[i].SetWaveform(1, ewf)

PRI SetRing(onoff) | i
  repeat i from 0 to MAX_VOICE
    sid[i].enableRingmod(false, onoff, false)
  byte[_cp][c_#ringmod] := onoff

PRI SetSync(onoff) | i
  repeat i from 0 to MAX_VOICE
    sid[i].enableSynchronization(false, onoff, false)
  byte[_cp][c_#oscsync] := onoff

PRI SetCutoff(val) | i, _vp
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    long[_vp][v_#l_cof] := val
    sid[i].setCutoff(val << 3)

PRI SetResonance(val) | i
  repeat i from 0 to MAX_VOICE
    sid[i].setResonance(val >> 3)

PRI SetModRate(rate) | i
  long[_cp][c_#l_lfo_rate] := rate
  repeat i from 0 to MAX_VOICE
    sid[i].setFreq(2, rate)

CON''### GLOBAL TG FUNCTIONS ###
PRI CalcFreq(cents) | tmp, key, oct, det, osh, frq
'
' Calculate frequency from absolute cents of semitone.
'
  tmp := cents  / 100           'semitones
  det := cents // 100           'detune 0..99
  oct := tmp  / 12              'octave
  key := tmp // 12              'key    0..11
  osh := 10 - oct
  if osh > 0
    frq := FTable[key] >>   osh
  elseif osh < 0
    frq := FTable[key] << ||osh
  else
    frq := FTable[key]
  if det > 0
    frq := (frq * CTable[det]) >> 12
  return frq

PRI MasterTuning(coarse, fine)
  g_tuning := (DecodeBip12(coarse) * 100) + DecodeBip100(fine)
  return g_tuning

PRI PolySyncLFO(n)
'
' Restart LFO for one SID channel.
' (used by sync-to-key mode)
'
  sid[n].enableTestChannel3(true)
  sid[n].enableTestChannel3(false)

PRI MonoSyncLFO | i
'
' Restart all LFOs, (nearly) in sync.
' (used by free running modes)
'
  repeat i from 0 to MAX_VOICE
    sid[i].enableTestChannel3(true)
  repeat i from 0 to MAX_VOICE
    sid[i].enableTestChannel3(false)

PRI DecorrelateLFO | i, tval
'
' Decorrelate LFSR values in LFOs.
' (used by S&H for maximum chaos ;-)
'
  repeat i from 0 to MAX_VOICE
    tval := DecoTable[i] ^ (cnt & $7F)
    sid[i].setFreq(2, tval)
  repeat i from 0 to MAX_VOICE
    sid[i].setFreq(2, long[_cp][c_#l_lfo_rate])

PRI LoadPatch(mch, n)
'
' Copy one patch from storage to working area,
' and activate its parameters by self-sending CCs.
'
  p_.Load(_pp, n)

  uart.str(1,string("Program Change: "))
  uart.str(1,_pp[p_#name])
  uart.str(1,string(13,10))

  '
  ' Patch loading from temp area to channel and voice structures
  ' is done by sending control changes locally, same as if the
  ' user dials in values on all the knobs.
  '
  '=========================================================================
  '-- Primary Set
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_DO1_WAVE,    byte[_pp][p_#do1_wf]     )
  CtlChange(mch, CC_DO2_WAVE,    byte[_pp][p_#do2_wf]     )
  CtlChange(mch, CC_DO2_INTVAL,  byte[_pp][p_#do2_intval] )
  CtlChange(mch, CC_DO2_DETUNE,  byte[_pp][p_#do2_detune] )
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_VCF_ATTACK,  byte[_pp][p_#vcf_attack] )
  CtlChange(mch, CC_VCF_DECAY,   byte[_pp][p_#vcf_decay]  )
  CtlChange(mch, CC_VCF_SUSTAIN, byte[_pp][p_#vcf_sustain])
  CtlChange(mch, CC_VCF_RELEASE, byte[_pp][p_#vcf_release])
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_LFO_WAVE,    byte[_pp][p_#lfo_wf]     )
  CtlChange(mch, CC_LFO_RATE,    byte[_pp][p_#lfo_rate]   )
  CtlChange(mch, CC_LFO_DELAY,   byte[_pp][p_#lfo_delay]  )
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_VCF_ENVAMT,  byte[_pp][p_#vcf_envamt] )
  CtlChange(mch, CC_VCF_CUTOFF,  byte[_pp][p_#vcf_cutoff] )
  CtlChange(mch, CC_VCF_RESON,   byte[_pp][p_#vcf_reson]  )
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_VCA_CTLBAL,  byte[_pp][p_#vca_ctlbal] )
  CtlChange(mch, CC_VCA_OSCBAL,  byte[_pp][p_#vca_oscbal] )
  '=========================================================================
  '-- Extended Set
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_DO1_ATTACK,  byte[_pp][p_#do1_attack] )
  CtlChange(mch, CC_DO1_DECAY,   byte[_pp][p_#do1_decay]  )
  CtlChange(mch, CC_DO1_SUSTAIN, byte[_pp][p_#do1_sustain])
  CtlChange(mch, CC_DO1_RELEASE, byte[_pp][p_#do1_release])
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_DO2_ATTACK,  byte[_pp][p_#do2_attack] )
  CtlChange(mch, CC_DO2_DECAY,   byte[_pp][p_#do2_decay]  )
  CtlChange(mch, CC_DO2_SUSTAIN, byte[_pp][p_#do2_sustain])
  CtlChange(mch, CC_DO2_RELEASE, byte[_pp][p_#do2_release])
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_VCO_V2EGAT,  byte[_pp][p_#do1_egttrk] )
  CtlChange(mch, CC_VCO_K2EGDT,  byte[_pp][p_#do2_egttrk] )
  CtlChange(mch, CC_DO1_INTVAL,  byte[_pp][p_#do1_intval] )
  CtlChange(mch, CC_DO1_DETUNE,  byte[_pp][p_#do1_detune] )
  '-------------------------------------------------------------------------
  CtlChange(mch, CC_VCF_KEYFLW,  byte[_pp][p_#vcf_keyflw] )
  CtlChange(mch, CC_VCF_L2EG,    byte[_pp][p_#vcf_velsns] ) 'FIXME: can't remember what the frak is this!
  CtlChange(mch, CC_MIX_CHORUS,  byte[_pp][p_#mix_chorus] )
  CtlChange(mch, CC_MIX_REVERB,  byte[_pp][p_#mix_reverb] )
  '=========================================================================
  '-- Misc inaccessible bits (?)
  '-------------------------------------------------------------------------
  'FIXME: this one should be relocated or renamed in patch.spin
  'needed an initial ModWheel setting to make some patches sound right
  CtlChange(mch, CC_MODWHEEL,    byte[_pp][p_#do2_lfoamt] )
  MasterTuning(byte[_pp][p_#do1_intval], byte[_pp][p_#do1_detune])
  '=========================================================================
  CtlChange(mch, CC_ALLNOTESOFF, 0)

PUB GMReset | i, _vp
'
' Reset tone generator to power-on defaults.
'
  g_rrcount := 0

  'this should be adjusted to match vco and vcf frequencies (fixed 24.8 value)
  g_vcfadj := 256

  long[_cp][c_#l_bending ] := 0
  byte[_cp][c_#brange    ] := 2

  CtlChange(0, CC_MODWHEEL,     0)
  CtlChange(0, CC_VOLUME,     100)
  CtlChange(0, CC_PANPOT,      64)
  CtlChange(0, CC_EXPRESSION, 127)
  CtlChange(0, CC_SUSPEDAL,     0)

  long[_cp][c_#l_lfoamt] := 0
  long[_cp][c_#l_envamt] := 0
  byte[_cp][c_#lfopol  ] := 0
  byte[_cp][c_#envpol  ] := 0

  byte[_cp][c_#ringmod] := 0
  byte[_cp][c_#oscsync] := 0
  byte[_cp][c_#sub_osc] := 0

  long[_cp][c_#l_lfo_step] := LFO_RAMP_MAX / (DTable[long[_cp][c_#l_lfo_delay] >> 3] * g_ramptk)

  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    byte[_vp][v_#state] := VOX_ST_FREE
    byte[_vp][v_#chn] := -1
    byte[_vp][v_#key] := -1
    byte[_vp][v_#vel] := 0
    long[_vp][v_#l_age] := 0
    byte[_vp][v_#flg] := 0
    byte[_vp][v_#lfo_state] := LFO_ST_IDLE

  repeat i from 0 to MAX_VOICE
'    sid[i].setRegister(24, 128)                 'OSC3 off (has no effect?!?)
    sid[i].setFilterType(true, false,false)     'low pass 12dB/oct
    sid[i].setFilterMask(true, true, false)     'VCO1 and VCO2, leave LFO alone
    sid[i].setCutoff(2047)
    sid[i].setResonance(0)
    sid[i].setVolume(10)
    sid[i].SetWaveform(2, WF_TRIANGLE)
    sid[i].setADSR(2,10,10,10,10)
    sid[i].noteOn(2, long[_cp][c_#l_lfo_rate])
    sid[i].noteOff(2)

  byte[_pp][p_#do1_intval] := 64
  byte[_pp][p_#do1_detune] := 64


  RetuneOSC2vsOSC1(0)

  'doing this so that edge triggered changes will
  'always succeed on first call to LoadPatch()
  repeat i from 0 to (p_#PATCH_SIZE - 1)
    byte[_pp][i] := -1

  PrgChange(0, 0)
  PrintMsg0(string("GM Reset"))

CON''### VOICE FUNCTIONS ###
PRI BumpVoiceAge(mch) | i, _vp
#ifdef MOD_DISABLE
'
' Advance all voice ages by one, to a max of 127.
'
' FIXME: moved to modulation task, only keep for devel
'
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if byte[_vp][v_#state] <> VOX_ST_FREE
      long[_vp][v_#l_age]++
    else
      byte[_vp][v_#l_age] := 0
#endif

PRI FindSameVoice(mch, key) | i, n, _vp
'
' Search for a voice already playing the same key.
' (unlikely with the current architecture)
'
  n := -1
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if byte[_vp][v_#state] <> VOX_ST_FREE
      if (byte[_vp][v_#chn] == mch) and (byte[_vp][v_#key] == key)
        n := i
        quit
  return n

PRI FindFreeVoice(mch, key) | i, n, _vp, tptr
'
' Search for a free voice that have nearest possible panning.
'
  n := -1
  tptr := @AP_Table + ((key >> 2) & $1C)
  repeat NUM_VOICES
    i := byte[tptr]
    _vp := voxp[i]
    if byte[_vp][v_#state] == VOX_ST_FREE
      n := i
      quit
    tptr++
  return n
{{
PRI FindFreeVoice(mch, key) | i, n, _vp
'
' Search for a free voice in round-robin order (key is ignored).
'
  n := -1
  c_rrcount := (c_rrcount + 1) // NUM_VOICES
  i := c_rrcount
  repeat NUM_VOICES
    _vp := @vox + (i * v_#VOICE_SIZE)
    if byte[_vp][v_#chn] == VF_FREE
      n := i
      quit
    i := (i + 1) // NUM_VOICES
  return n
}}

PRI FindOldestVoice(mch) | i, n, _vp, tval, t_maxv
  n := -1
  t_maxv := 0
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if byte[_vp][v_#state] == VOX_ST_RLSE
      tval := long[_vp][v_#l_age]
      if tval > t_maxv
        t_maxv := tval
        n := i
  return n

PRI StealVoice(mch) | i, j, n, _vp, tval, t_minv, t_mini
'
' Find second to lowest playing note, and "steal" it
'
  'find the lowest key
  t_minv := 128
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    tval := byte[_vp][v_#key]
    if tval < t_minv
      t_minv := tval
      t_mini := i
  'search again for 2nd-to-lowest
  t_minv := 128
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    tval := byte[_vp][v_#key]
    if (i <> t_mini) and (tval < t_minv)
      t_minv := tval
      n := i
  return n

PRI FreeVoice(n) | _vp
'
' Mark a voice as free for reuse.
'
  _vp := voxp[n]
  if byte[_vp][v_#state] <> VOX_ST_FREE
    byte[_vp][v_#state] := VOX_ST_FREE
    g_currpoly--

PRI AllocVoice(mch, key) | n, _vp
'
' Obtain a new or recycled voice.
'
#ifdef MOD_DISABLE
  BumpVoiceAge(mch) 'FIXME: remove this
#endif
  '--------------------------------------------------------------------
  '1st: look for a voice with the same key, either playing or released
  n := FindSameVoice(mch, key)
  if (n <> -1)
    _vp := voxp[n]
    ReleaseVoice(n)
    byte[_vp][v_#chn] := mch
    return n
  '--------------------------------------------------------------------
  '2nd: search for free voice (round-robin OR nearest pan position)
  n := FindFreeVoice(mch, key)
  if (n <> -1)
    _vp := voxp[n]
    g_currpoly++
    byte[_vp][v_#chn] := mch
    return n
  '--------------------------------------------------------------------
  '3rd: search again for oldest releasing note (no key this time)
  n := FindOldestVoice(mch)
  if (n <> -1)
    _vp := voxp[n]
    byte[_vp][v_#chn] := mch
    return n
  '--------------------------------------------------------------------
  '4th: if still no voice available, kill second to lowest note playing
  n := StealVoice(mch)
  ReleaseVoice(n)
  _vp := voxp[n]
  byte[_vp][v_#chn] := mch
  return n
  '--------------------------------------------------------------------

PRI RetuneFilter(n) | _vp, tval
'
' Recalculate filter cutoff for a voice.
'
  _vp := voxp[n]
  tval := (byte[_pp][p_#vcf_cutoff] - 64) * 100
  if byte[_pp][p_#vcf_keyflw]
    tval += byte[_vp][v_#key] * 100
  tval += long[_vp][v_#l_cfm] * 100
  if tval < 0
    tval := 0
  if tval > 12799
    tval := 12799
  tval := (tval * g_vcfadj) >> 8  'fixed 24.8 adjust factor
  long[_vp][v_#l_cof] := CalcFreq(tval) >> 7
  sid[n].setCutoff(long[_vp][v_#l_cof])
  sid[n].setResonance(byte[_pp][p_#vcf_reson])

PRI RetuneVoice(n) | _vp, cents, mdest, freq
'
' Recalculate oscillator pitches for a voice.
'
  _vp := voxp[n]
  cents := long[_vp][v_#l_frq]
  mdest := byte[_pp][p_#lfo_mdest]
  cents += long[_cp][c_#l_bending]
  if (mdest < 64)
    cents += long[_vp][v_#l_det]
  freq := CalcFreq(cents)
  sid[n].setFreq(0, freq)
  cents += long[_cp][c_#l_do2_detune]
  if (mdest => 64)
    cents += long[_vp][v_#l_det]
  freq := CalcFreq(cents)
  sid[n].setFreq(1, freq)

PRI RemixVoice(n) | _vp, tval1, tval2
'
' Recalculate levels for a voice.
'
''  sid[n].setVolume(byte[_cp][c_#express] >> 3)
''  return
  'NOTE: velocity is handled in NoteOn() using SID volume
  _vp := voxp[n]
'  tval := (c_volume * c_express * g_volume) >> 5
'  tval := (c_volume * c_express) << 2
'  s_depth[n] := 64011 - tval
'  s_depth[n] := 0
'  s_angle[n] := ((c_panpot << 1) + 1) << 8
'  s_angle[n] := $8000
  tval1 := byte[_cp][c_#express]
  tval1 := (tval1 * (100 - long[_cp][c_#l_vca_ctlbal])) / 200
  tval2 := byte[_vp][v_#vel]
  tval2 := (tval2 * (100 + long[_cp][c_#l_vca_ctlbal])) / 200
  sid[n].setVolume((tval1 + tval2) >> 3)

PRI ReleaseVoice(n) | _vp
'
' Release a voice (begin EG release stage).
'
  _vp := voxp[n]
  sid[n].noteOff(0)
  sid[n].noteOff(1)
  sid[n].noteOff(2)
  byte[_vp][v_#state] := VOX_ST_GOFF
'DumpRegs(n)

PRI SetupEnvelope(n, balchg) | i, _vp, mf, ck, rf, t0, t1, t2, t3, _ps[3]
  _vp := voxp[n]
  '--------------------------------------------------------------------------------------
  '-- attack --
  t0 := DecodeBip100(byte[_pp][p_#do1_egttrk])          'FIXME: rename in Patch as vel2eat
  if t0 <> 0
    mf := ((64 - byte[_vp][v_#vel]) * t0) ~> 8 '-25..0..+25 approx
    ck := (DecodeUni10(byte[_pp][p_#vcf_attack]) << 2) + mf
    rf := cnt & 1
    t1 := ((ck + rf) #> 0) <# 40 'knob, vsens, add random bit (0,1)
    t2 := ((ck - rf) #> 0) <# 40 'knob, vsens, sub random bit (0,1)
    t3 := ((ck     ) #> 0) <# 40 'knob, vsens
    byte[_vp][v_#do1_attack ] := ET_E12_Table[t1]
    byte[_vp][v_#do2_attack ] := ET_E12_Table[t2]
    byte[_vp][v_#vcf_attack ] := ET_E12_Table[t3]
  else 'no scaling
    ck := DecodeUni10(byte[_pp][p_#vcf_attack  ])
    t1 := ET_E3_Table[ck]
    byte[_vp][v_#do1_attack ] := t1
    byte[_vp][v_#do2_attack ] := t1
    byte[_vp][v_#vcf_attack ] := t1
  '--------------------------------------------------------------------------------------
  '-- decay & release --
  t0 := DecodeBip100(byte[_pp][p_#do2_egttrk])          'FIXME: rename in Patch as key2edt
  if t0 <> 0
    mf := ((64 - byte[_vp][v_#key]) * t0) ~> 8 '-25..0..+25 approx
    ck := (DecodeUni10(byte[_pp][p_#vcf_decay  ]) << 2) + mf
    t1 := ((ck + 1) #> 0) <# 40 'knob, keyflw and nudge up
    t2 := ((ck - 1) #> 0) <# 40 'know, keyflw and nudge down
    t3 := ((ck    ) #> 0) <# 40 'knob, keyflw and leave as is
    byte[_vp][v_#do1_decay  ] := ET_E12_Table[t1]
    byte[_vp][v_#do2_decay  ] := ET_E12_Table[t2]
    byte[_vp][v_#vcf_decay  ] := ET_E12_Table[t3]
    ck := (DecodeUni10(byte[_pp][p_#vcf_release]) << 2) + mf
    t1 := ((ck + 1) #> 0) <# 40 'knob, keyflw and nudge up
    t2 := ((ck - 1) #> 0) <# 40 'know, keyflw and nudge down
    t3 := ((ck    ) #> 0) <# 40 'knob, keyflw and leave as is
    byte[_vp][v_#do1_release] := ET_E12_Table[t1]
    byte[_vp][v_#do2_release] := ET_E12_Table[t2]
    byte[_vp][v_#vcf_release] := ET_E12_Table[t3]
  else 'no scaling
    ck := DecodeUni10(byte[_pp][p_#vcf_decay  ])
    t1 := ET_E3_Table[ck]
    byte[_vp][v_#do1_decay  ] := t1
    byte[_vp][v_#do2_decay  ] := t1
    byte[_vp][v_#vcf_decay  ] := t1
    ck := DecodeUni10(byte[_pp][p_#vcf_release])
    t1 := ET_E3_Table[ck]
    byte[_vp][v_#do1_release] := t1
    byte[_vp][v_#do2_release] := t1
    byte[_vp][v_#vcf_release] := t1
  '--------------------------------------------------------------------------------------
  '-- sustain --
  mf := DecodeBip100(byte[_pp][p_#vca_oscbal ])
  t3 := 100             '100..100..100
  t1 := (t3 - mf) <# t3 ' 0 ..100..100
  t2 := (t3 + mf) <# t3 '100..100..  0
  ck := DecodeUni10( byte[_pp][p_#vcf_sustain])

  if RETRIG_ON_SUS_CHANGE
    _ps[0] := byte[_vp][v_#do1_sustain]
    _ps[1] := byte[_vp][v_#do2_sustain]
    _ps[2] := byte[_vp][v_#vcf_sustain]

  byte[_vp][v_#do1_sustain] := (ck * t1) ~> 6
  byte[_vp][v_#do2_sustain] := (ck * t2) ~> 6
  byte[_vp][v_#vcf_sustain] := (ck * t3) ~> 6
  'got a range that's already compatible with registers, no table here

  if RETRIG_ON_SUS_CHANGE
    if byte[_vp][v_#do1_sustain] <> _ps[0]
      sid[n].retrigger(0)
    if byte[_vp][v_#do2_sustain] <> _ps[1]
      sid[n].retrigger(1)
    if byte[_vp][v_#vcf_sustain] <> _ps[2]
      sid[n].retrigger(2)

  '--------------------------------------------------------------------------------------
  sid[n].setADSR(0, byte[_vp][v_#do1_attack ], byte[_vp][v_#do1_decay  ], {
                 }  byte[_vp][v_#do1_sustain], byte[_vp][v_#do1_release])
  sid[n].setADSR(1, byte[_vp][v_#do2_attack ], byte[_vp][v_#do2_decay  ], {
                 }  byte[_vp][v_#do2_sustain], byte[_vp][v_#do2_release])
  sid[n].setADSR(2, byte[_vp][v_#vcf_attack ], byte[_vp][v_#vcf_decay  ], {
                 }  byte[_vp][v_#vcf_sustain], byte[_vp][v_#vcf_release])

CON''### CHANNEL FUNCTIONS ###
PRI RetuneOSC2vsOSC1(mch) | tval
  tval := DecodeBip12(byte[_pp][p_#do2_intval]) * 100   'get interval in cents
  if byte[_cp][c_#sub_osc]                              'if enabled...
    tval -= 1200                                        '...add SubOsc setting
  tval += DecodeBip100(byte[_pp][p_#do2_detune])        'add detune in cents
  long[_cp][c_#l_do2_detune] := tval

PRI RetuneFilters(mch) | i, _vp
'
' Retune VCF for all voices in a channel.
'
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if (byte[_vp][v_#chn] == mch)
      RetuneFilter(i)

PRI RetuneVoices(mch) | i, _vp
'
' Redo tuning for all voices in a channel.
'
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if (byte[_vp][v_#chn] == mch)
      RetuneVoice(i)

PRI RemixVoices(mch) | i, _vp
'
' Redo mixing for all voices in a channel.
'
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if (byte[_vp][v_#chn] == mch)
      RemixVoice(i)

PRI ReleaseHeldVoices(mch) | i, _vp
'
' Release all voices in a channel with HELD flag set.
'
  repeat i from 0 to MAX_VOICE
    _vp := voxp[i]
    if byte[_vp][v_#chn] == mch
      if byte[_vp][v_#flg] & VF_HELD
        byte[_vp][v_#flg] := 0
        ReleaseVoice(i)
'        FreeVoice(i)

PRI SetupEnvelopes(mch, balchg) | i, _vp
  repeat i from 0 to MAX_VOICE
'    _vp := voxp[i]
'    if (byte[_vp][v_#chn] == mch)
      SetupEnvelope(i, balchg)

CON''### MESSAGE FUNCTIONS ###
PRI EnvTimeTrack(val, key) | rval
'
' Scale Envelope times by key
'
'  tval := CalcFreq(key * 100)
'  oval := (ival << 12) / tval
'  oval := (ival * 10) / key
'  oval := (ival * 8) / key
  rval := ((val + 64 - key) #> 0) <# 127
'PrintMsg3(string("bloody key track: "), key, ival, oval)
  return (rval >> 3)
{
PRI EnvOscBalance(ival, num) | tval, oval, mult
  oval := ival >> 3
  tval := DecodeBip100(byte[_pp][p_#vca_oscbal])
  mult := 100
  case num
    0: if tval > 0
         mult -= tval
    1: if tval < 0
         mult += tval
  oval := (oval * mult) / 100
  return oval

PRI DecodeWaveDO1(val)

PRI DecodeWaveDO2(val)

PRI DecodeWaveLFO(val)
}

PRI DecodeUni10(val) | tval
'
' decode an unipolar control from raw 0..127 range to 0..5..10
' has dead (5) and saturation zones (0 and 10)
'
  case val
    0..13:    tval := 0               'zero 13
    14..57:   tval := (val - 3) / 11  'lrng 44 (4*11)
    58..69:   tval := 5               'cntr 12
    70..113:  tval := (val - 4) / 11  'hrng 44 (4*11)
    114..127: tval := 10              'vmax 13
    other:    tval := 5
  return tval

PRI DecodeBip10(val) | tval
'
' decode a bipolar control from raw 0..127 range to -10..0..+10
' has dead (0) and saturation zones (-10 and +10)
' (saturation zones has same range of std steps in this decoder)
'
  case val
    0..5:     tval := -10             'sat-  6
    6..59:    tval := (val - 65) / 6  '-rng 54 (9*6)
    60..67:   tval := 0               'dead  8
    68..121:  tval := (val - 62) / 6  '+rng 54 (9*6)
    122..127: tval :=  10             'sat+  6
    other:    tval := 0
  return tval

PRI DecodeBip12(val) | tval
'
' decode a bipolar control from raw 0..127 range to -100..0..+100
' has dead (0) and saturation zones (-12 and +12)
'
  case val
    0..5:     tval := -12             'sat-  6
    6..60:    tval := (val - 65) / 5  '-rng 55 (11*5)
    61..66:   tval :=   0             'dead  6
    67..121:  tval := (val - 62) / 5  '+rng 55 (11*5)
    122..127: tval :=  12             'sat+  6
    other:    tval := 0
  return tval

PRI DecodeBip100(val) | tval
'
' decode a bipolar control from raw 0..127 range to -100..0..+100
' has dead (0) and saturation zones (-100 and +100)
'
  case val
    0..9:     tval := -100            'sat- 10
    10..58:   tval := (val - 59) << 1 '-rng 49 (49*1)
    59..68:   tval := 0               'dead 10
    69..117:  tval := (val - 68) << 1 '+rng 49 (49*1)
    118..127: tval :=  100            'sat+ 10
    other:    tval := 0
  return tval

PUB NoteOff(mch, key, vel) | n, _vp
'
' MIDI message 80 kk vv - Note Off
'
  n := FindSameVoice(mch, key)
  if n <> -1
    if byte[_cp][c_#sustped] < 64
      ReleaseVoice(n)           'if no sustain pedal, release the voice
    else
      _vp := voxp[n]
      byte[_vp][v_#flg] |= VF_HELD       'else flag it as "held"
'  VoiceToLeds

PUB NoteOn(mch, key, vel) | n, _vp, cents, tval
'
' MIDI message 90 kk vv - NoteOn
'
  if vel == 0
    NoteOff(mch, key, 64)
    return
  n := AllocVoice(mch, key)
  _vp := voxp[n]
  byte[_vp][v_#key] := key
  byte[_vp][v_#vel] := vel
  byte[_vp][v_#flg] := 0

  long[_vp][v_#l_age] := 0
  byte[_vp][v_#state] := VOX_ST_GATE

  cents := (key * 100) + g_tuning
  long[_vp][v_#l_frq] := cents    'base pitch => key + (global tuning)

  RetuneOSC2vsOSC1(mch)

  RemixVoice(n)

  if byte[_pp][p_#do1_wf] == WF_VELPWM
    if byte[_pp][p_#do2_wf] == WF_EFFECTS
      SetDuty2(vel)
    else
      SetDuty1(vel)
  SetupEnvelope(n, false)

  cents += long[_cp][c_#l_bending]    'add current bender value
  tval := CalcFreq(cents)
  sid[n].noteOn(0, tval)
  cents += long[_cp][c_#l_do2_detune] 'add offset for OSC2
  tval := CalcFreq(cents)
  sid[n].noteOn(1, tval)
  tval := long[_cp][c_#l_lfo_rate]
  sid[n].noteOn(2, tval)

  if byte[_cp][c_#lfo_wave] == WF_POLYTRIG
    sid[n].enableTestChannel3(true)
    sid[n].enableTestChannel3(false)

'  VoiceToLeds

PUB PolyPress(mch, key, val)
'
' MIDI message A0 kk vv - Polyphonic Pressure
'
{{
  This is not supported, and not even planned.
  I never seen it implemented by any real device :-)
}}

PRI _SingleKnobSetXlat(ctl)
  '"PIC retlw"-ish lookup table!
  case ctl
    '=========================================================================
    CC_DO1_WAVE:    return CC_DO1_ATTACK
    CC_DO2_WAVE:    return CC_DO1_DECAY
    CC_DO2_INTVAL:  return CC_DO1_SUSTAIN
    CC_DO2_DETUNE:  return CC_DO1_RELEASE
    '-------------------------------------------------------------------------
    CC_VCF_ATTACK:  return CC_DO2_ATTACK
    CC_VCF_DECAY:   return CC_DO2_DECAY
    CC_VCF_SUSTAIN: return CC_DO2_SUSTAIN
    CC_VCF_RELEASE: return CC_DO2_RELEASE
    '-------------------------------------------------------------------------
    CC_LFO_WAVE:    return CC_VCO_V2EGAT
    CC_LFO_RATE:    return CC_VCO_K2EGDT
    CC_LFO_DELAY:   return CC_DO1_INTVAL
    CC_VCF_ENVAMT:  return CC_DO1_DETUNE
    '-------------------------------------------------------------------------
    CC_VCF_CUTOFF:  return CC_VCF_KEYFLW
    CC_VCF_RESON:   return CC_VCF_L2EG
    CC_VCA_CTLBAL:  return CC_MIX_CHORUS
    CC_VCA_OSCBAL:  return CC_MIX_REVERB
    '=========================================================================
    other:          return CC_ALLNOTESOFF

PRI _WaveKnobCombiner(wf1, wf2) | pw1, pw2, cw1, cw2, t_ring, t_sync, p_sosc, t_sosc
  pw1 := byte[_cp][c_#do1_wave]
  pw2 := byte[_cp][c_#do2_wave]
  if wf1 == -1
    cw1 := pw1
  else
    cw1 := wf1
  if wf2 == -1
    cw2 := pw2
  else
    cw2 := wf2
  byte[_cp][c_#do1_wave] := cw1
  byte[_cp][c_#do2_wave] := cw2
  word[_cp][c_#w_do1_duty] := $0800
  word[_cp][c_#w_do2_duty] := $0800
  t_ring := false
  t_sync := false
  t_sosc := false
  p_sosc := byte[_cp][c_#sub_osc]
  case cw2
    WF_EFFECTS:    '### xxx/EFX (special combination of knobs)
      case cw1
        WF_TRIANGLE: '### TRI/EFX => TRI/TRI + ring mod
          wf1 := cw1 '\_
          wf2 := cw1 '/  triangle
          t_ring := true
        WF_SAWTOOTH: '### SAW/EFX => SAW/SAW + osc sync
          wf1 := cw1 '\_
          wf2 := cw1 '/  sawtooth
          t_sync := true
        WF_SQUARE:   '### SQR/EFX => PW 33% + PW 25%
          wf1 := cw1 '\_
          wf2 := cw1 '/  pulse
          word[_cp][c_#w_do1_duty] := $0AAA
          word[_cp][c_#w_do2_duty] := $0555

'          word[_cp][c_#w_do1_duty] := $02AA ' 3rd harmonic \_ not really harmonics... sort of
'          word[_cp][c_#w_do2_duty] := $0200 ' 4th harmonic /  well, let's see how it sounds ;-)
        WF_VELPWM:   '### PWM/EFX => TRI/VarSlope, velocity controlled
          wf1 := WF_PULSE
          wf2 := WF_PULSE '<- dynamically changed by NoteOn() !
          'wf1 := WF_OSCOFF
        WF_RANDOM:   '### RND/EFX => RND+SIN (sine from wavetable)s
          wf1 := WF_RANDOM
          wf2 := WF_WTABLE0
        other:       'should never get here anyway...
          wf1 := WF_OSCOFF
          wf2 := WF_OSCOFF
    WF_SUBOSC:     '### xxx/SUB => xxx/TRI (triangle one octave down)
      case cw1
        WF_VELPWM:
          wf1 := WF_PULSE '<- dynamically change PW in NoteOn()!
        other:
          wf1 := cw1
      wf2 := WF_TRIANGLE
      t_sosc := true
    other:         '### xxx/yyy (any other combination)
      case cw1
        WF_VELPWM:
          wf1 := WF_PULSE '<- dynamically change PW in NoteOn()!
          wf2 := WF_SQUARE '<- REMOVE ME
        other:
          wf1 := cw1
          wf2 := cw2

  if (cw1 <> pw1) or (cw2 <> pw2)
    SetRing(t_ring)
    SetSync(t_sync)
    SetWaveform1(wf1, word[_cp][c_#w_do1_duty])
    SetWaveform2(wf2, word[_cp][c_#w_do2_duty])
  if t_sosc <> p_sosc
    if t_sosc
      byte[_cp][c_#sub_osc]  := $7F
    else
      byte[_cp][c_#sub_osc]  := $00
    RetuneOSC2vsOSC1(0)

PUB CtlChange(mch, ctl, val) | i, tval, pval
'
' MIDI message B0 nn vv - Control Change
'
  'FIXME: just for debugging, later leave it in, but with config option
  if g_altknobs
    ctl := _SingleKnobSetXlat(ctl)

  case ctl
    '=========================================================================
    '-- channel controls - basic
    '-------------------------------------------------------------------------
    CC_MODWHEEL:
      byte[_cp][c_#modwheel] := val
      tval := (val * val)                 'using a strong exponential function here:
      tval := val + ((tval * tval) >> 17) 'musical vibrato in the first half of control travel,
      long[_cp][c_#l_lfoamt] := tval      'and arcade style effects with more deeper settings
    CC_VOLUME:
      byte[_cp][c_#volume] := val
      RemixVoices(mch)
    CC_PANPOT:
      byte[_cp][c_#panpot] := val
      RemixVoices(mch)
    CC_EXPRESSION:
      byte[_cp][c_#express] := val
      RemixVoices(mch)
    CC_SUSPEDAL:
      byte[_cp][c_#sustped] := val
      if (val < 64)
        ReleaseHeldVoices(mch)  'on pedal depressed, release all "held" voices
    'FIXME: add CC_DATAENT_LSB and CC_DATAENT_MSB
    '=========================================================================
    '-- patch controls - primary set
    '-------------------------------------------------------------------------
    CC_DO1_WAVE:
      byte[_pp][p_#do1_wf] := val
      case val
        0..25:    tval := WF_TRIANGLE
        26..50:   tval := WF_SAWTOOTH
        51..76:   tval := WF_SQUARE
        77..101:  tval := WF_VELPWM
        102..127: tval := WF_RANDOM
        other:    tval := WF_OSCOFF
      _WaveKnobCombiner(tval, -1)
    CC_DO2_WAVE:
      byte[_pp][p_#do2_wf] := val
      case val
        0..25:    tval := WF_TRIANGLE
        26..50:   tval := WF_SAWTOOTH
        51..76:   tval := WF_SQUARE
        77..101:  tval := WF_EFFECTS
        102..127: tval := WF_SUBOSC
        other:    tval := WF_OSCOFF
      _WaveKnobCombiner(-1, tval)
    CC_DO2_INTVAL:
      byte[_pp][p_#do2_intval] := val
      RetuneOSC2vsOSC1(mch)
    CC_DO2_DETUNE:
      byte[_pp][p_#do2_detune] := val
      RetuneOSC2vsOSC1(mch)
    '-------------------------------------------------------------------------
    CC_VCF_ATTACK:
      tval := DecodeUni10(val)
      byte[_pp][p_#do1_attack] := val
      byte[_pp][p_#do2_attack] := val
      byte[_pp][p_#vcf_attack] := val
      SetupEnvelopes(0, false)
    CC_VCF_DECAY:
      tval := DecodeUni10(val)
      byte[_pp][p_#do1_decay] := val
      byte[_pp][p_#do2_decay] := val
      byte[_pp][p_#vcf_decay] := val
      SetupEnvelopes(0, false)
    CC_VCF_SUSTAIN:
      tval := DecodeUni10(val)
      byte[_pp][p_#do1_sustain] := val
      byte[_pp][p_#do2_sustain] := val
      byte[_pp][p_#vcf_sustain] := val
      SetupEnvelopes(0, true)
    CC_VCF_RELEASE:
      tval := DecodeUni10(val)
      byte[_pp][p_#do1_release] := val
      byte[_pp][p_#do2_release] := val
      byte[_pp][p_#vcf_release] := val
      SetupEnvelopes(0, false)
    '-------------------------------------------------------------------------
    CC_LFO_WAVE:
      byte[_pp][p_#lfo_wf] := val
      pval := byte[_cp][c_#lfo_wave]
      case val
        0..25:    tval := WF_POLYTRIG
        26..50:   tval := WF_RAMPDOWN
        51..76:   tval := WF_SINE
        77..101:  tval := WF_RAMPUP
        102..127: tval := WF_SAMNHOLD
        other:    tval := WF_OSCOFF
      if pval <> tval
        byte[_cp][c_#lfo_wave] := tval
        case tval
          WF_POLYTRIG:
            tval := WF_WTABLE0  'WF_TRIANGLE
            byte[_cp][c_#lfopol  ] := 1
          WF_RAMPDOWN:
            tval := WF_SAWTOOTH
            byte[_cp][c_#lfopol  ] := 1
            MonoSyncLFO
          WF_SINE:
            tval := WF_TRIANGLE 'WF_WTABLE0
            byte[_cp][c_#lfopol  ] := 0
            MonoSyncLFO
          WF_RAMPUP:
            tval := WF_SAWTOOTH
            byte[_cp][c_#lfopol  ] := 0
            MonoSyncLFO
          WF_SAMNHOLD:
            tval := WF_RANDOM
            byte[_cp][c_#lfopol  ] := 0
            DecorrelateLFO
          other:
            tval := WF_OSCOFF
        SetWaveform3(tval)
    CC_LFO_RATE:
      byte[_pp][p_#lfo_rate] := val
      SetModRate(val + 1)
    CC_LFO_DELAY:
      byte[_pp][p_#lfo_delay] := val
      tval := DecodeUni10(val)
      tval := DTable[tval]      'delay in msec
      'step for ramp phase, incr per 256 ticks
      long[_cp][c_#l_lfo_step ] := LFO_RAMP_MAX / (tval * g_ramptk * 3)
      'time of delay phase, in cnt ticks
      long[_cp][c_#l_lfo_delay]:= (tval * g_ramptk) << 8
      'CHECKME: effect of moving the knob on already running LFO ?!?
    '-------------------------------------------------------------------------
    CC_VCF_ENVAMT:
      byte[_pp][p_#vcf_envamt] := val
      tval := DecodeBip100(val)
      byte[_cp][c_#envpol  ] := 0
      long[_cp][c_#l_envamt] := (((tval * tval * tval) / 10000) + tval) ~> 1 'y=(x^3+x)/2
    CC_VCF_CUTOFF:
      byte[_pp][p_#vcf_cutoff] := val
#ifdef MOD_DISABLE
      RetuneFilters(mch)
#endif
    CC_VCF_RESON:
      byte[_pp][p_#vcf_reson]  := val
      SetResonance(val)
    '-------------------------------------------------------------------------
    CC_VCA_CTLBAL:
      byte[_pp][p_#vca_ctlbal] := val
      long[_cp][c_#l_vca_ctlbal] := DecodeBip100(val)
      RemixVoices(mch)
    CC_VCA_OSCBAL:
      byte[_pp][p_#vca_oscbal] := val
      long[_cp][c_#l_vca_oscbal] := DecodeBip100(val)
      SetupEnvelopes(0, true)
    '=========================================================================
    '-- patch controls - alternate set
    '-------------------------------------------------------------------------
    CC_DO1_ATTACK:
      byte[_pp][p_#do1_attack ] := val
    CC_DO1_DECAY:
      byte[_pp][p_#do1_decay  ] := val
    CC_DO1_SUSTAIN:
      byte[_pp][p_#do1_sustain] := val
    CC_DO1_RELEASE:
      byte[_pp][p_#do1_release] := val
    '-------------------------------------------------------------------------
    CC_DO2_ATTACK:
      byte[_pp][p_#do2_attack ] := val
    CC_DO2_DECAY:
      byte[_pp][p_#do2_decay  ] := val
    CC_DO2_SUSTAIN:
      byte[_pp][p_#do2_sustain] := val
    CC_DO2_RELEASE:
      byte[_pp][p_#do2_release] := val
    '-------------------------------------------------------------------------
    CC_VCO_V2EGAT:
      byte[_pp][p_#do1_egttrk ] := val 'FIXME: rename!!!
    CC_VCO_K2EGDT:
      byte[_pp][p_#do2_egttrk ] := val 'FIXME: rename!!!
    CC_DO1_INTVAL:
      byte[_pp][p_#do1_intval ] := val
      'redo global tuning!!!
    CC_DO1_DETUNE:
      byte[_pp][p_#do1_detune ] := val
      'redo global tuning!!!
    '-------------------------------------------------------------------------
    CC_VCF_KEYFLW:
      byte[_pp][p_#vcf_keyflw] := val
    CC_VCF_L2EG:
      PrintMsg1(string("DUNNO WHAT THE FRAK = "), val)
    CC_MIX_CHORUS:
      byte[_pp][p_#mix_chorus  ] := val
    CC_MIX_REVERB:
      byte[_pp][p_#mix_reverb  ] := val
    '=========================================================================
    CC_ALLNOTESOFF:
      ReleaseHeldVoices(mch)
      repeat i from 0 to MAX_VOICE
        ReleaseVoice(i)
'        FreeVoice(i)
        repeat tval from 0 to 2
          sid[i].noteOff(tval)
      g_currpoly := 0
      uart.str(1,string("*** ALL NOTES OFF ***",13,10))
    '-- channel controls - mode/global
    'FIXME: add:
    '  CC_RPN_LSB and CC_RPN_MSB
    '  CC_NRPN_LSB and CC_NRPN_MSB
    '  CC_ALL_NOTES_OFF and more...
    '-------------------------------------------------------------------------
    other:
      if tval < 0
        uart.str(1,string("WARNING: unknwon control change mch="))
        if mch < 10
          uart.tx(1,48)
        uart.dec(1,mch)
        uart.str(1,string(" CTL=0x"))
        uart.hex(1,ctl, 2)
        uart.str(1,string(" VAL=0x"))
        uart.hex(1,val, 2)
        uart.tx(1,13)
        uart.tx(1,10)

PUB PrgChange(mch, prg)
'
' MIDI message C0 nn - Program Change
'
  'FIXME: remove this stuff, it depends on the controller
'  waitcnt(cnt+clkfreq) 'my MIDI keyboard is set to send all control values on program change
'  uart.rxflush(0)      'will take care later, now it would just pollute debug output :-D

  g_altknobs := false
  LoadPatch(mch, prg & 7)

  if prg & 8
    g_altknobs := true
  else
    g_altknobs := false

PUB ChanPress(mch, val)
'
' MIDI message D0 vv - Channel Pressure
'
  'FIXME: not sure this is the best use of Channel AT (whitout doing things
  '       in an overly complicated manner) have to rethink about it!
{
  if (byte[_pp][p_#do2_wf] == WF_EFFECTS) and (byte[_pp][p_#do1_wf] == WF_PULSE)
    SetDuty1(val)
    long[_cp][c_#l_do2_detune] := ub2si(byte[_pp][p_#do2_detune]) + (val >> 3)
    RetuneVoices(0)
}

PUB PitchBend(mch, lsb, msb) | t_bender
'
' MIDI message E0 ll mm - Pitch Bender
'
  'NOTE: lsb is only used to fix range simmetry
  t_bender := GetSymmVal(lsb, msb)
  'current bending in cents
  long[_cp][c_#l_bending] := ((byte[_cp][c_#brange] * 100) * t_bender) / 64
  RetuneVoices(mch)
'  DumpAll

PUB System(sys) | t_dat, t_val
'
' MIDI message F0 [xx ..] - System Message
'
  case sys
    $F0: 'System exclusive
      'we only recognize GM Master Volume
      'and attempt to skip the rest
      t_dat := uart.rx(0)
      if t_dat == $7F
        t_dat := uart.rx(0)
        if t_dat == $7F
          t_dat := uart.rx(0)
          if t_dat == $04
            t_dat := uart.rx(0)
            if t_dat == $01
              t_dat := uart.rx(0) 'LSB
              t_val := uart.rx(0) 'MSB
              t_dat := uart.rx(0) 'EOX
              if t_dat == $F7
                'yeeeaaah it's a boy!!!
                'pretend this is an expression pedal message (FIXME: remove later)
                byte[_cp][c_#express] := t_val
                RemixVoices(0)
      'if not already there, wait for EOX
      repeat while t_dat <> $F7
        t_dat := uart.rx(0)
    other:
      'TODO: skip system stuff with length <> 1 !!!
      PrintMsg1(string("Sys message byte: "), sys)

CON''### MODULATION ###
{
  TODO:
    - create some dedicated global vars to avoid excessive stack usage for locals (?)
    - insert calculated wait after measuring max cycle time (to get constant modulation rate) <- DONE!
}
PRI NormalizeMod(val, pol) | lsb, msb, rval
'
' given an unsigned modulator with range 0..255
' return it as signed (and symmetric) -64..0..+64
'
  msb := val >> 1
  lsb := (val & 1) * 127
  rval := GetSymmVal(lsb, msb)
  if pol
    rval := -rval
  return rval

PRI LFO_ModeSwitch(lfo_mode, lfo_state) | rval
'
' manage behaviour of "LFO amount" SW envelope after gate off events
'
  case lfo_mode
    0: 'TRIGGER MODE: ignore gate off, continue with delay/ramp/cruise
      rval := lfo_state   'keep current state
    1: 'COAST MODE: mantain whatever level we've reached so far
      rval := LFO_ST_HOLD 'hold current level
    2: 'OUT OF FUEL MODE: when no more gated, level will always fall to zero
      if lfo_state <> LFO_ST_WAIT
        rval := LFO_ST_FALL
      else 'ST_RAMP, ST_HOLD
        rval := LFO_ST_HOLD
    3: 'CIRCUS mode: if we miss grip on the handle, we'll fall down to ground ;-D
      if lfo_state == LFO_ST_RAMP
        rval := LFO_ST_FALL
      else 'ST_WAIT, ST_HOLD
        rval := LFO_ST_HOLD
  return rval

PRI HandleContMod | i, _vp, _sp, tval, t_lfoval, t_envval, t_lfoamt, t_envamt, t_delta, t_moden
'
' called periodically by ModTask, this is where modulation happens
'
'  serial.str(string("  E3 ="))
'  t_lfoamt := c_lfoamt
  t_envamt := long[_cp][c_#l_envamt]
  t_delta := GetElapsedTicks

  repeat i from 0 to MAX_VOICE
    _vp := voxp[i] 'pointer to voice structure #i
    _sp := regp[i] 'pointer to SID #i registers

    '-- read modulation sources from SID
    t_lfoval := byte[_sp][30]
    t_envval := byte[_sp][31]
    'show OSC3 and ENV3 MSBs on LEDs
'    OUTA[19] := (t_lfoval & $80) >> 7
'    OUTA[18] := (t_envval & $80) >> 7

    '-- voice state machine (partial, other bits outside)
    case byte[_vp][v_#state]
      VOX_ST_FREE:
        t_moden := false
      VOX_ST_GATE:
        'transitional state
        long[_vp][v_#l_age] := 0
        long[_vp][v_#l_cnt] := 0
        byte[_vp][v_#lfo_state] := LFO_ST_WAIT
        byte[_vp][v_#state] := VOX_ST_PLAY
        t_moden := true
      VOX_ST_PLAY:
        'long[_vp][v_#l_age]++
        t_moden := true
      VOX_ST_GOFF:
        'transitional state
        'long[_vp][v_#l_age] <<= 1
        tval := byte[_vp][v_#lfo_state]
        if tval == LFO_ST_WAIT
          long[_vp][v_#l_cnt] := 0 'since we're sharing same counter for delay and amount!
'        byte[_vp][v_#lfo_state] := LFO_ModeSwitch(LFO_DEPTH_BEHAVIOUR, tval)
        byte[_vp][v_#lfo_state] := LFO_ST_HOLD
        byte[_vp][v_#state] := VOX_ST_RLSE
        t_moden := true
      VOX_ST_RLSE:
        long[_vp][v_#l_age]++
        if t_envval == 0
          byte[_vp][v_#state] := VOX_ST_SHUT
        t_moden := true
      VOX_ST_SHUT:
        'transitional state, make decision about LFO behaviour
{       '(opt) shut down SSP input?
        s_input[i] := 0
        'in this case, we also need the following moved to NoteOn():
        s_input[i] := regp[i] + 32 '28 - CHECK OFFSET!
}
        'long[_vp][v_#l_age] <<= 2
        byte[_vp][v_#state] := VOX_ST_FREE
        byte[_vp][v_#lfo_state] := LFO_ST_IDLE
        if g_currpoly > 0
          g_currpoly--
        t_moden := false
      other:
'{
        cogstop(0)
        waitcnt(cnt+clkfreq)
        uart.str(1,string(13,10,"*** FRAK !!! ***"))
        uart.str(1,string("voice #"))
        uart.dec(1,i)
        uart.str(1,string(" has invalid state 0x"))
        uart.hex(1,byte[_vp][v_#state],2)
        uart.str(1,string(" !!!",13,10))
        uart.str(1,string("*** FRAK !!! ***",13,10))
        waitcnt(cnt+clkfreq)
        cogstop(7)
'}
        byte[_vp][v_#state] := VOX_ST_FREE

    'only modulate for VS_PLAY and VS_RLSE states
    if t_moden
      '-- step LFO state machine
      case byte[_vp][v_#lfo_state]
        LFO_ST_IDLE:
          t_lfoamt := 0
        LFO_ST_WAIT:
          long[_vp][v_#l_cnt] += t_delta
          if long[_vp][v_#l_cnt] => long[_cp][c_#l_lfo_delay]
            long[_vp][v_#l_cnt] := 0
            byte[_vp][v_#lfo_state] := LFO_ST_RAMP
          t_lfoamt := 0
        LFO_ST_RAMP:
          long[_vp][v_#l_cnt] += (t_delta * long[_cp][c_#l_lfo_step]) >> 8
          if long[_vp][v_#l_cnt] => LFO_RAMP_MAX
            long[_vp][v_#l_cnt] := LFO_RAMP_MAX
            byte[_vp][v_#lfo_state] := LFO_ST_HOLD
          t_lfoamt := ((long[_vp][v_#l_cnt] >> 16) * long[_cp][c_#l_lfoamt]) >> 8
        LFO_ST_HOLD:
          t_lfoamt := ((long[_vp][v_#l_cnt] >> 16) * long[_cp][c_#l_lfoamt]) >> 8
        LFO_ST_FALL:
          long[_vp][v_#l_cnt] -= (t_delta * long[_cp][c_#l_lfo_step]) >> 8
          if long[_vp][v_#l_cnt] =< 0
            long[_vp][v_#l_cnt] := 0
            byte[_vp][v_#lfo_state] := LFO_ST_HOLD
          t_lfoamt := ((long[_vp][v_#l_cnt] >> 16) * long[_cp][c_#l_lfoamt]) >> 8
        other: byte[_vp][v_#lfo_state] := LFO_ST_IDLE

      '-- apply modulation to VCOs and VCF
'      long[_vp][v_#l_det] := (NormalizeMod(t_lfoval+0, byte[_cp][c_#lfopol]) * t_lfoamt) / 81
      long[_vp][v_#l_det] := (NormalizeMod(t_lfoval+0, byte[_cp][c_#lfopol]) * t_lfoamt * 51) ~> 12
      RetuneVoice(i)
'      long[_vp][v_#l_cfm] := (NormalizeMod(t_envval+8, byte[_cp][c_#envpol]) * t_envamt) / 76
      long[_vp][v_#l_cfm] := (NormalizeMod(t_envval+8, byte[_cp][c_#envpol]) * t_envamt * 54) ~> 12
      RetuneFilter(i)

PUB ModTask | t_delay
'
' modulation thread, running in a separate COG
'
'  OUTA[19..16] := %0000
'  DIRA[19..16] := %1111
'  modblink := 0
'  modcount := MODBRATE

  t_delay := cnt
  repeat
    HandleContMod
'    if (--modcount == 0)
'      modcount := MODBRATE
'      OUTA[19] := modblink
'      modblink ^= 1
'      OUTA[17] := 1
'    else
'      OUTA[17] := 0

    if MOD_SPEED_LIMIT == 1
      t_delay += g_dticks
      waitcnt(t_delay)

CON''### DATA TABLES ###
DAT'Part 1 - Frequency Tables
{
Reference Constants:
  semi = 1.0594630943592952645618252949463
  cent = 1.0005777895065548592967925757932

Top octave in Hz*256 (?):
        2143237,2270680,2405702,2548752,2700309,2860878
        3030994,3211227,3402176,3604480,3818814,4045892
}
'======================================================================
'-- top octave table
'
' in   : 0..11 (semitones)
' out  : frequency (hertz (?), fixed int 28.4)
' size : 48 bytes
'
{
'1) Hertz (x16) - DO NOT USE, it is off by nearly a semitone!
FTable  long  $020B40,$022A5E,$024B54,$026E41,$029341,$02BA75
        long  $02E3FD,$030FFE,$033E9C,$037000,$03A454,$03DBC4
}
'2) Recalculated using Rval = (Fkey * 16777216) / Fclk
FTable  long  $0224AB,$02454B,$0267DC,$028C7B,$02B347,$02DC62
        long  $0307EF,$033613,$0366F5,$039ABF,$03D19E,$040BC0
'
'----------------------------------------------------------------------
'-- detune table
'
' in   : 0..127 (cents)
' out  : multiplier (fixed int 20.12)
' size : 512 bytes
'
CTable  long  $1000,$1002,$1005,$1007,$1009,$100C,$100E,$1011
        long  $1013,$1015,$1018,$101A,$101C,$101F,$1021,$1024
        long  $1026,$1028,$102B,$102D,$1030,$1032,$1034,$1037
        long  $1039,$103A,$103E,$1040,$1043,$1045,$1048,$104A
        long  $104C,$104F,$1051,$1054,$1056,$1058,$105B,$105D
        long  $1060,$1062,$1065,$1067,$1069,$106C,$106E,$1071
        long  $1073,$1076,$1078,$107A,$107D,$107F,$1082,$1084
        long  $1087,$1089,$108C,$108E,$1090,$1093,$1095,$1098
        long  $109A,$109D,$109F,$10A2,$10A4,$10A7,$10A9,$10AB
        long  $10AE,$10B0,$10B3,$10B5,$10B8,$10BA,$10BD,$10BF
        long  $10C2,$10C4,$10C7,$10C9,$10CC,$10CE,$10D1,$10D3
        long  $10D6,$10D8,$10DB,$10DD,$10E0,$10E2,$10E5,$10E7
        long  $10EA,$10EC,$10EF,$10F1,$10F4,$10F6,$10F9,$10FB
        long  $10FE,$1100,$1103,$1105,$1108,$110A,$110D,$110F
        long  $1112,$1114,$1117,$1119,$111C,$111E,$1121,$1123
        long  $1126,$1129,$112B,$112E,$1130,$1133,$1135,$1138
'
'======================================================================
DAT'Part 2 - Envelope Time Tables
'======================================================================
'-- an attempt to bring the ADSR times to a more sane progression,
'   using resistor series as a model for exponential scaling
'----------------------------------------------------------------------
' in   :  0..10 (index)
' out  :  0..15 (SID reg value, roughly following the E3 series)
' size : 12 bytes
'
ET_E3_Table   byte       0, 1, 3, 5, 8, 9,10,12,13,14,15,-1
'----------------------------------------------------------------------
' in   :  0..20 (index)
' out  :  0..15 (SID reg value, roughly following the E6 series)
' size : 24 bytes
'
ET_E6_Table   byte       0, 0, 1, 2, 3, 4, 5, 6, 8, 8, 9, 9
              byte      10,11,12,12,13,13,14,15,15,-1,-1,-1
'----------------------------------------------------------------------
' in   :  0..40 (index)
' out  :  0..15 (SID reg value, roughly following the E12 series)
' size : 44 bytes
'
ET_E12_Table  byte       0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7
              byte       8, 8, 8, 9, 9, 9, 9,10,10,10,11,11,12,12,12,12
              byte      13,13,13,13,14,14,15,15,15,-1,-1,-1
'
'======================================================================
DAT'Part 3 - Misc Tables
'======================================================================
'-- voice alloc priority table (panorama position affinity order)
'
' in   : 0..7 (scaled key, x4->0..28, select row)
' in   : 0..3 (index to scan columns)
' out  : 0..3 (voice number)
' size : 32 bytes
'
'NOTE1: only valid for 4 voices, and if StereoSpatializer
'       panpot is fixed to 0x0000,0x5555,0xAAAA,0xFFFF !
'
AP_Table      byte      0,1,2,3   'key range 00..0F
              byte      0,1,2,3   'key range 10..1F
              byte      1,0,2,3   'key range 20..2F
              byte      1,2,0,3   'key range 30..3F
              byte      2,1,3,0   'key range 40..4F
              byte      2,3,1,0   'key range 50..5F
              byte      3,2,1,0   'key range 60..6F
              byte      3,2,1,0   'key range 70..7F
'
'----------------------------------------------------------------------
'-- slope table (velocity to variable slope using wavetables)
'
' in   : 0..15   (scaled velocity)
' out  : waveform number
' size : 16 bytes
'
STable  byte  WF_TRIANGLE, WF_TRIANGLE, WF_TRIANGLE, WF_TRIANGLE '\_ first half is fixed
        byte  WF_TRIANGLE, WF_TRIANGLE, WF_TRIANGLE, WF_TRIANGLE '/  at 50% (triangle)
        byte  WF_TRIANGLE, WF_WTABLE1,  WF_WTABLE1,  WF_WTABLE1  '\_ second half scale
        byte  WF_WTABLE2,  WF_WTABLE2,  WF_WTABLE3,  WF_SAWTOOTH '/  from 50% to 100%
'
'----------------------------------------------------------------------
'-- delay table for LFO (same times as SID envelope attack stage)
'
' in   :  0..10   (delay setting)
' out  :  2..8000 (msec)
' size : 32 bytes
'
'NOTE: as with EGs, some values have been skipped
'
DTable  word  2,8,24,56,100,250,500,1000,3000,5000,8000,-1
'
'======================================================================

{{
  The following stuff needs reordering, most may be deleted after debug
}}

'some primes to use as frequency for oscillator decorrelation
DecoTable     word      2017,2027,2029,2039

{
WNames0 'standard  (VCO)
  byte  "OFF",0,"TRI",0,"SAW",0,"WT0",0
  byte  "PUL",0,"WT1",0,"WT2",0,"WT3",0
  byte  "RND",0,"PWM",0,"EFX",0,"SUB",0
  byte  "PSK",0,"RDN",0,"SIN",0,"RSV",0
WNames1 'aliases 1 (LFO)
  byte  "OFF",0,"TRI",0,"RUP",0,"WT0",0
  byte  "SQR",0,"WT1",0,"WT2",0,"WT3",0
  byte  "S&H",0,"PWM",0,"EFX",0,"SUB",0
  byte  "PSK",0,"RDN",0,"SIN",0,"RSV",0
WNames2 'aliases 2 (VSL)
  byte  "OFF",0,"V50",0,"V99",0,"SIN",0
  byte  "PUL",0,"V70",0,"V85",0,"V95",0
  byte  "RND",0,"$90",0,"$A0",0,"$B0",0
  byte  "$C0",0,"$D0",0,"$E0",0,"$F0",0

VSNames byte  "[FREE]",0,0
        byte  "[GATE]",0,0
        byte  "[PLAY]",0,0
        byte  "[GOFF]",0,0
        byte  "[RLSE]",0,0
        byte  "[SHUT]",0,0
        byte  "[-??-]",0,0

MSNames byte  "[IDLE]",0,0
        byte  "[WAIT]",0,0
        byte  "[RAMP]",0,0
        byte  "[HOLD]",0,0
        byte  "[FALL]",0,0
        byte  "[-!!-]",0,0
        byte  "[-??-]",0,0
}
CON''### FOOTER ###
{{WRITE ME!}}
