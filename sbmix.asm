;
; Sound Blaster 16 mixer control utility
;
; Copyright (c) 2024 Vitaly Sinilin
;

cpu 8086
[map all sbmix.map]

%define abs(addr)             (addr+PSP_SIZE-$$)
%define swp_word(word)        (((word) >> 8) | (((word) & 0FFh) << 8))
%define make_ext(addr, flags) swp_word((addr) | (((flags) | STR_FLAG_EXT) << 8))
%define ref(addr)             make_ext(abs(addr), 0)
%define spaces(addr, n)       make_ext(abs(addr), (n << 4))
%define space(addr)           spaces(addr, 1)
%define padding(len)          swp_word((len) | (HT << 8))

%macro str 1-*
.size		db	.len
.data		equ	$
%rep %0
%ifstr %1
		db	%1
%elif %1 = CR
		db	%1
%else
		dw	%1
%endif
%rotate 1
%endrep
.len		equ	$ - .data
%endmacro

%macro putch 1
		mov	dl, %1
		call	_putch
%endmacro

%macro puts 1
		mov	dx, %1
		call	_puts
%endmacro

%macro die 1
		mov	dx, %1
		jmp	_die
%endmacro

PSP_SIZE	equ	100h

HT		equ	9
LF		equ	10
CR		equ	13

STR_FLAG_EXT	equ	80h
STR_SPACE_MASK	equ	70h
STR_PTR_MASK	equ	0Fh

MIXER_ADDR_PORT	equ	04h	; Mixer write-only Address Port, base-relative
MIXER_DATA_PORT	equ	05h	; Mixer read/write Data Port, base-relative
MASTER_VOL_REG	equ	22h	; Master volume register index
WAVE_VOL_REG	equ	04h	; Wave volume register index
FM_VOL_REG	equ	26h	; FM synth volume register index
LINE_VOL_REG	equ	2Eh	; Line input volume register index
CD_VOL_REG	equ	28h	; CD input volume register index
MIC_VOL_REG	equ	0Ah	; Mic input volume register index
PCSPKR_VOL_REG	equ	3Bh	; PC Speaker volume register index

SB_DSPRST	equ	06h	; DSP reset port, base-relative
SB_DSPRD	equ	0Ah	; DSP read data port, base-relative
SB_DSPWR	equ	0Ch	; DSP write data/command port, base-relative
SB_DSPAV	equ	0Eh	; DSP data available status port, base-relative

DSP_GETVER	equ	0E1h	; Get DSP version number command
DSPTIMEOUT	equ	4FFh	; Timeout for DSP

section .text

		org	PSP_SIZE

		;
		; Fill BSS with 0FFh bytes indicating uninitialized data
		;
init_bss:
		mov	bx, __bss_size
.fill_bss:	dec	bx
		mov	byte [__bss+bx], 0FFh
		jnz	.fill_bss
		; fall through

main:
		call	parse_cmdline
		call	find_blaster
		call	parse_blaster
		call	check_blaster

		;
		; Alter mixer volume values.
		;
		mov	bx, 0FF0Fh		; BH = unchanged value (FFh)
						; BL = maximum value (15)
		mov	si, stereo_chan_cnt
.alter_next:	mov	ah, [master_left+si-1]
		cmp	ah, bh
		je	.skip_channel
		call	norm_value
		mov	cl, 4
		shl	ah, cl
		xchg	cx, ax
		mov	ah, [master_right+si-1]
		call	norm_value
		or	ah, ch
		mov	al, [stereo_reg_tbl+si-1]
		call	set_mixer_reg
.skip_channel:	dec	si
		jnz	.alter_next

		mov	ah, [mic_mono]		; Alter mic volume
		cmp	ah, bh			; if requested.
		je	.alter_pc_spkr
		mov	bl, 7			; BL = maximum value
		call	norm_value
		mov	al, MIC_VOL_REG
		call	set_mixer_reg

.alter_pc_spkr:	mov	ah, [pc_spkr_mono]	; Alter PC Speaker volume
		cmp	ah, bh			; if requested.
		je	.show
		mov	bl, 3			; BL = maximum value
		call	norm_value
		mov	cl, 6
		shl	ah, cl
		mov	al, PCSPKR_VOL_REG
		call	set_mixer_reg

		;
		; Show mixer volume values if not quiet.
		;
.show:		cmp	byte [quiet], 0
		jne	.success

		mov	dx, blaster
		call	print_label
		puts	env_blaster
		call	newline

		mov	dx, dsp_version
		call	print_label
		mov	al, [dsp_ver_num+1]
		call	print_byte_dec
		putch	'.'
		mov	al, [dsp_ver_num]
		call	print_byte_dec02
		call	newline

		xor	si, si
.show_next:	shl	si, 1
		mov	dx, [stereo_lbl_tbl+si]
		shr	si, 1
		call	print_vol_label_lr
		mov	al, [stereo_reg_tbl+si]
		call	get_mixer_reg
		call	print_stereo
		inc	si
		cmp	si, stereo_chan_cnt
		jne	.show_next

		mov	dx, mic
		call	print_vol_label
		mov	al, MIC_VOL_REG
		call	get_mixer_reg
		and	al, 7
		call	print_byte_dec2_nl

		mov	dx, pc_spkr
		call	print_vol_label
		mov	al, PCSPKR_VOL_REG
		call	get_mixer_reg
		mov	cl, 6
		shr	al, cl
		call	print_byte_dec2_nl

.success:	mov	al, 0
		; fall through

exit:		mov	ah, 4Ch			; exit
		int	21h

_die:		call	_puts
		call	newline
		mov	al, 1
		jmp	exit

		;
		; Make sure volume value in AH is not greater than BL.
		;
norm_value:
		cmp	ah, bl
		jna	.valid
		mov	ah, bl
.valid:		ret

		;
		; Check if BLASTER is set and card is actually present and
		; of supported type.
		;
check_blaster:
		cmp	word [base], 0FFFFh
		jne	.check_type
		die	invalid_blaster
.dsp_failure:	die	hw_io_error

.check_type:	mov	al, [type]
		cmp	al, 6			; 6 - SB16, also SB32,
		jae	.get_dsp_ver		; AWE32/64, ViBRA.
.unsup_card:	die	sb_16_required

.get_dsp_ver:	call	reset_dsp
		jc	.dsp_failure
		call	get_dsp_ver
		jc	.dsp_failure
		mov	bx, ax
		call	get_dsp_ver
		jc	.dsp_failure
		cmp	bx, ax
		jne	.dsp_failure		; conflict with another card

.check_dsp_ver:	cmp	ah, 4
		jb	.unsup_card
		mov	[dsp_ver_num], ax
		ret

		;
		; Parse command line
		;
parse_cmdline:
		xor	bh, bh
		mov	bl, [80h]		; command line len
		mov	si, bx
		neg	bx
		add	si, 81h			; command line offset
.next_option:	call	skip_spaces
		jnz	.check_option
.exit		ret

.help:		puts	help1
		die	help2

.check_option:	cmp	byte [si+bx], '/'
		jne	.check_master
		inc	bx
		jz	.exit
		mov	al, [si+bx]

		cmp	al, '?'
		je	.help
		call	upcase
		cmp	al, 'H'
		je	.help
		cmp	al, 'Q'
		jne	.is_channel
		mov	byte [quiet], 1
		inc	bx
		jmp	.next_option

.is_channel:	mov	di, opts_len
.next_channel:	cmp	al, [opts+di-1]
		je	.known_opt
		dec	di
		jnz	.next_channel
		jmp	.invalid_opt

.known_opt:	lea	di, [wave_left+di-1]
		jmp	.parse_volume

.check_master:	mov	al, [si+bx]
		call	parse_dec_digit
		jnc	.parse_master
.invalid_opt:	die	invalid_params

.parse_master:	dec	bx
		mov	di, master_left

.parse_volume:	mov	byte [di], 0		; set volume to 0
		inc	bx			; skip option letter
		call	skip_spaces		; skip optional spaces between
		jz	.fnlize_volume		; option name and value

.next_digit:	mov	al, [si+bx]
		call	parse_dec_digit
		jc	.check_stereo
		xchg	al, [di]
		mov	cl, 10
		mul	cl
		add	[di], al
		inc	bx
		jnz	.next_digit
.fnlize_volume:	call	is_stereo
		jnc	.exit
.copy_lr:	mov	al, [di]		; copy left channel volume
		mov	[di+chan_cnt], al	; to right channel
		jmp	.next_option

.check_stereo:	call	skip_spaces		; skip optional spaces before
		jz	.fnlize_volume		; comma
		call	is_stereo
		jnc	.next_option
		cmp	byte [si+bx], ','
		jne	.copy_lr
		add	di, chan_cnt		; switch DI to right channel
		jmp	.parse_volume

		;
		; Skips all spaces in [SI+BX] (BX shall be negative or zero).
		; Output: [SI+BX] points to the first non-space character or
		; ZF is set if no more characters (i.e. BX reached zero value).
		;
skip_spaces:
		or	bx, bx
		jz	.done
.check_char:	cmp	byte [si+bx], ' '
		jne	.done
		inc	bx
		jnz	.check_char
.done:		ret

		;
		; Find BLASTER in environment and copy its nul-terminated
		; value into env_blaster. If not found, an empty string is
		; copied.
		;
find_blaster:
		mov	es, [2Ch]		; env segment
		mov	di, 0
		cld
.next_var:	mov	cx, blaster_len
		mov	si, blaster+1		; skip len byte
		repe cmpsb
		jne	.find_nul
		cmp	byte [es:di], '='
		jne	.find_nul
		inc	di
.copy:		mov	bx, -1
.next_char:	inc	bx
		mov	al, [es:di+bx]
		mov	[env_blaster.data+bx], al
		cmp	al, 0
		jne	.next_char
		mov	[env_blaster.len], bl
		ret

.find_nul:	mov	cl, [es:di]
		inc	di
		jcxz	.got_nul
		jmp	.find_nul

.got_nul:	cmp	byte [es:di], 0
		jne	.next_var
		jmp	.copy

		;
		; Parse BLASTER value
		;
parse_blaster:
		mov	si, env_blaster.data
		cld
.read_param:	lodsb
.next_param:	cmp	al, 0
		jne	.check_char
		ret

.check_char:	call	upcase
		cmp	al, 'A'
		jne	.is_type
		mov	word [base], 0
.next_adigit:	lodsb
		call	parse_dec_digit
		jnc	.mul_base
.check_hex:	call	upcase
		cmp	al, 'A'
		jb	.next_param
		cmp	al, 'F'
		ja	.next_param
		sub	al, 'A'-10
.mul_base:	mov	cl, 4
		shl	word [base], cl
		xor	ah, ah
		add	word [base], ax
		jmp	.next_adigit

.is_type:	cmp	al, 'T'
		jne	.read_param
		mov	byte [type], 0
.next_tdigit:	lodsb
		call	parse_dec_digit
		jc	.next_param
		xchg	al, [type]
		mov	cl, 10
		mul	cl
		add	[type], al
		jmp	.next_tdigit

		;
		; Is DI pointing to a stereo volume variable?
		; Set CF if stereo.
		;
is_stereo:
		cmp	di, master_left
		jb	.mono
		cmp	di, master_left+stereo_chan_cnt
		jae	.mono
		ret
.mono:		clc
		ret

print_vol_label_lr:
		call	_puts
		mov	di, bx			; save len
		puts	volume
		add	di, bx			; save len
		puts	left_right
		add	bx, di
		jmp	print_padding

print_vol_label:
		call	_puts
		mov	di, bx			; save len
		puts	volume
		add	bx, di
		jmp	print_padding

print_label:
		call	_puts
		; fall through

print_padding:
		neg	bx
		add	bx, 31
		mov	cx, bx
		mov	dl, '.'
		call	print_nchars
		putch	' '
		ret

		;
		; Print string (with extended sequences) from DS:DX.
		;
_puts:
		push	si
		mov	si, dx
		inc	si			; skip len byte
		xor	bx, bx
.next_char:	mov	dx, [si+bx]
		cmp	dl, HT
		jne	.check_ext
		call	print_spaces
		jmp	.skip2

.check_ext:	test	dl, STR_FLAG_EXT	; is it an extended sequence?
		jnz	.check_space
		cmp	dl, CR			; is it a CR?
		jne	.not_cr
		call	newline
		jmp	.skip1

.not_cr:	call	_putch
		jmp	.skip1

.check_space:	and	dl, STR_SPACE_MASK
		jz	.puts
		mov	cl, 4
		shr	dl, cl
		mov	dh, dl
		call	print_spaces
.puts:		mov	dx, [si+bx]
		xchg	dh, dl
		and	dh, STR_PTR_MASK
		push	bx
		call	_puts
		pop	bx
.skip2:		inc	bx
.skip1:		inc	bx
.loop:		cmp	bl, [si-1]
		jb	.next_char
		pop	si
		ret

		;
		; Print DH spaces.
		;
print_spaces:
		xor	ch, ch
		mov	cl, dh
		mov	dl, ' '
		; fall through

		;
		; Print DL CX times.
		;
print_nchars:
.next:		call	_putch
		loop	.next
		ret

		;
		; Print CR+LF sequence.
		;
newline:
		putch	CR
		mov	dl, LF
		; fall through

		;
		; Print char stored in DL.
		;
_putch:	
		mov	ah, 6
		int	21h
		ret

		;
		; Print stereo value in AL as "<0..15>,<0..15>".
		;
print_stereo:
		push	ax
		mov	cl, 4
		shr	al, cl
		call	print_byte_dec2
		putch	','
		pop	ax
		and	al, 0Fh
		; fall through

print_byte_dec2_nl:
		call	print_byte_dec2
		jmp	newline

		;
		; Naively convert char in AL to upper case.
		;
upcase:
		cmp	al, 'a'
		jb	.exit			; already upper case
		sub	al, 20h
.exit		ret

		;
		; Convert ASCII decimal digit in AL to its value.
		; Input: AL - digit
		; Output: AL - decimal value
		;
parse_dec_digit:
		cmp	al, '0'
		jb	.failure
		cmp	al, '9'
		ja	.failure
		sub	al, '0'
		clc
		ret

.failure:	stc
		ret

		;
		; Print decimal digit in AL.
		;
print_dec_digit:
		mov	dl, al
		add	dl, '0'
		jmp	_putch

		;
		; Print right adjusted zero padded decimal value of AL
		; in a field of width 2.
		;
print_byte_dec02:
		mov	dx, 130h		; DH = 1, DL = 30h ('0')
		jmp	_print_byte_dec

		;
		; Print right adjusted decimal value of AL in a field
		; of width 2.
		; 
print_byte_dec2:mov	dx, 120h		; DH = 1, DL = 20h (' ')
		jmp	_print_byte_dec

print_byte_dec:	mov	dh, 0
		; fall through

		;
		; Print right adjusted DL padded decimal value of AL
		; in a field of width DH.
		; Input: AL - byte, DH - width, DL - padding char
		;
_print_byte_dec:xor	ah, ah
		mov	cl, 10
		div	cl
		push	ax
		cmp	al, 0
		je	.units
		dec	dh
		call	_print_byte_dec
.units:		cmp	dh, 0			; print padding
		jle	.print_dec_digit
		call	_putch
		dec	dh
		jmp	.units
.print_dec_digit:	pop	ax
		mov	al, ah
		call	print_dec_digit
		xor	dh, dh			; reset padding from rest digits
		ret

		;
		; Select SB mixer register.
		; Input: AL - register index
		; Output: DX - mixer data port
		;
sel_mixer_reg:
		mov	dx, [base]
		add	dx, MIXER_ADDR_PORT
		out	dx, al
		inc	dx		; mixer data port
		ret

		;
		; Set SB mixer register.
		; Input: AL - register index
		;        AH - value
		;
set_mixer_reg:
		call	sel_mixer_reg
		mov	al, ah
		out	dx, al
		ret

		;
		; Read SB mixer register.
		; Input: AL - register index
		; Output: AL - register value
		;
get_mixer_reg:
		call	sel_mixer_reg
		in	al, dx
		ret

		;
		; Write SB DSP Data/Command register, set CF if timeout.
		; Input: AL - register index
		;
write_dsp_reg:
		push	ax
		mov	dx, [base]
		add	dx, SB_DSPWR	; DSP Data/Command (W) and Write Buffer Status (R)
		mov	cx, DSPTIMEOUT
.wait:		in	al, dx
		test	al, 80h
		loopnz	.wait		; wait until bit 7 = 0
		jnz	.failure
		pop	ax
		out	dx, al
		clc
		ret
.failure:	stc
		ret

		;
		; Read SB DSP Data (Lo) and Data Available (Hi) register,
		; set CF if timeout.
		;
read_dsp_reg:
		mov	dx, [base]
		add	dx, SB_DSPAV	; DSP Data Available
		mov	cx, DSPTIMEOUT
		mov	ah, 80h		; bit mask
.wait:		in	al, dx
		test	al, ah
		loopz	.wait		; wait until bit 7 = 1
		mov	ah, al		; return also status byte
		jz	.failure	; timeout, return nonzero
		sub	dx, (SB_DSPAV - SB_DSPRD) ; DSP Data Read
		in	al, dx
		clc
		ret
.failure:	stc
		ret

		;
		; Reset DSP version, set CF if failed.
		;
reset_dsp:
		mov	dx, [base]
		add	dx, SB_DSPRST
		mov	al, 1
		out	dx, al		; write 1 to DSP Reset port
		in	al, dx
		in	al, dx
		in	al, dx
		in	al, dx		; wait a bit (3.3 microseconds)
		mov	al, 0
		out	dx, al		; then write 0
		call	read_dsp_reg	; read the answer
		jc	.failure
		cmp	al, 0AAh
		jne	.failure
		clc
		ret
.failure:	stc
		ret

		;
		; Return DSP version, set CF if failed.
		;
get_dsp_ver:
		mov	al, DSP_GETVER
		call	write_dsp_reg	; send command
		jc	.failure
		call	read_dsp_reg	; read version major
		xchg	cx, ax		; save
		jc	.failure	; timeout
		call	read_dsp_reg	; read version minor
		jc	.failure
		mov	ah, cl		; return major:minor in AX
		clc
		ret
.failure:	stc
		ret

quiet		db	0
dsp_ver_num	dw	0

stereo_reg_tbl	db	MASTER_VOL_REG
		db	WAVE_VOL_REG
		db	FM_VOL_REG
		db	LINE_VOL_REG
		db	CD_VOL_REG

stereo_lbl_tbl	dw	master
		dw	wave
		dw	fm_synth
		dw	line
		dw	cd

opts		db	'WFLCXS'
opts_len	equ	$-opts
blaster		str	'BLASTER environment variable'
blaster_len	equ	7		; len of word BLASTER
dsp_version	str	'DSP version'
master		str	'Master'
wave		str	'Wave'
fm_synth	str	'FM synth'
line		str	'Line'
cd		str	'CD'
left_right	str	' (Left, Right)'
mic		str	'Mic'
pc_spkr		str	'PC Speaker'
volume		str	' volume'
correctly	str	'correctly!'
sb_16_required	str	'SoundBlaster 16 or compatible required!'
hw_io_error	str	'Hardware I/O error!'
invalid_params	str	'Invalid command-line parameters! (', \
			ref(progname), ' /? for help.)'
progname	str	'SBMIX'
title		str	'Version 06-Nov-2024  SB16 mixer control utility'
copyright	str	'Copyright (c)'
vs		str	'2024 Vitaly Sinilin'
bttr		str	'1997-2020 BTTR Software'
master_opt	str	'<master>'
stereo_opt	str	'= <both> | <left>,<right> ='
volume_comma	str	'volume,'
wave_opt	str	'<wave>',
fm_opt		str	'<fm>'
cd_opt		str	'<cd>'
input		str	'input'
line_opt	str	'<line>',
mic_opt		str	'<mic>',
mono_opt	str	'= <mono>'
pcspkr_opt	str	'<pcspkr>'
range0_15	str	'0..15'

invalid_blaster	str	ref(blaster), ' is not set', space(correctly)

help1 str \
	ref(progname), space(title), CR, \
	ref(copyright), space(vs), CR, \
	ref(copyright), space(bttr), CR, \
	CR, \
	'Usage:', space(progname), ' [', ref(master_opt), ']', \
		' [/W', ref(wave_opt), ']', \
		' [/F', ref(fm_opt), ']', \
		' [/C', ref(cd_opt), ']', \
		' [/L', ref(line_opt), ']', CR, \
	padding(13), '[/X', ref(mic_opt), '] [/S', ref(pcspkr_opt), ']', CR, \
	spaces(progname, 7), ' [/H|/?]', CR, \
	CR, \
	spaces(master_opt, 2), space(stereo_opt), \
		' master', \
		space(volume_comma), spaces(range0_15, 5), CR, \
	spaces(wave_opt, 2), spaces(stereo_opt, 3), \
		' wave (DSP)', \
		space(volume_comma), space(range0_15), CR, \
	spaces(fm_opt, 2), spaces(stereo_opt, 5), \
		space(fm_synth), \
		space(volume_comma), spaces(range0_15, 3), CR, \
	spaces(cd_opt, 2), spaces(stereo_opt, 5), \
		space(cd), space(input), \
		space(volume_comma), spaces(range0_15, 3), CR, \
	spaces(line_opt, 2), spaces(stereo_opt, 3), \
		space(line), space(input), \
		space(volume_comma), space(range0_15), CR, \
	spaces(mic_opt, 2), spaces(mono_opt, 4), padding(18), '=', \
		space(mic), space(input), \
		space(volume_comma), padding(2), '0..7', CR, \
	spaces(pcspkr_opt, 2), space(mono_opt), padding(18), '=', \
		space(pc_spkr), \
		space(volume_comma), ' 0..3', CR, \
	CR

help2 str \
	'Note:', space(blaster), ' must be set', space(correctly), CR, \
	CR,\
	'Examples:', CR, \
	spaces(progname, 2), ' 3,5', CR, \
	spaces(progname, 2), ' 9 /W15,5 /F15,5 /C5,15 /L1 /X1', CR, \
	spaces(progname, 2)

section .bss follows=.text nobits

__bss		equ	$
base		resw	1
type		resb	1
master_left	resb	1
wave_left	resb	1
fm_left		resb	1
line_left	resb	1
cd_left		resb	1
stereo_chan_cnt	equ	$-master_left
mic_mono	resb	1
pc_spkr_mono	resb	1
chan_cnt	equ	$-master_left
master_right	resb	1
wave_right	resb	1
fm_right	resb	1
line_right	resb	1
cd_right	resb	1
env_blaster:
.len		resb	1
.data		resb	256 ; nul-terminated
__bss_size	equ	$-__bss
