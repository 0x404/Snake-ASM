.586
.model flat, stdcall
option casemap :none


include windows.inc
include masm32.inc
include gdi32.inc
include user32.inc
include kernel32.inc
include msvcrt.inc

includelib masm32.lib
includelib gdi32.lib
includelib user32.lib
includelib kernel32.lib
includelib msvcrt.lib


WinMain PROTO :DWORD,:DWORD,:DWORD,:DWORD
WndProc PROTO :DWORD,:DWORD,:DWORD,:DWORD
TopXY  PROTO  :DWORD,:DWORD
random   PROTO  :DWORD   ; 生成一个[0, ARG]之间的随机数 
initGame    PROTO       ; 初始化游戏参数变量
randomApple PROTO       ; 随机生成一个食物，更新全局appleX, appleY，并按照概率可能生成爱心、星星、炸弹  
randomHeart PROTO       ; 随机生成一个爱心，更新全局heartX, heartY
randomStar  PROTO       ; 随机生成一个星星，更新全局starX, starY
randomBoom  PROTO       ; 随机生成一个炸弹，更新全局boomX, boomY

compare	PROTO: ptr byte,:ptr byte
dtoc	PROTO 
GetLine PROTO:  HANDLE,:ptr byte,:DWORD;读取history文件的每一个行，文件内已排好序
Fileprocess PROTO: ptr byte,:DWORD;读取文件有三种操作类型，0时读取第一行，1时读取前20行，2时插入对应位置
sprintf PROTO C :ptr sbyte, :VARARG
strcmp PROTO C :ptr sbyte, :VARARG
strcat PROTO C :ptr sbyte, :VARARG  
strcpy PROTO C :ptr sbyte, :VARARG   
strlen PROTO C :ptr sbyte, :VARARG 
printf PROTO C :dword,   :VARARG
.data
    WM_FINISH     equ WM_USER+100h
    
    SNAKE       equ 101     ; 代表蛇的位图ID
    APPLE       equ 102     ; 代表食物的位图ID
    STAR        equ 104     ; 代表星星的位图ID
    HEART       equ 103     ; 代表爱心的位图ID
    BOOM        equ 105     ; 代表炸弹的位图ID

    UP          equ 1       ; 上方向
    DOWN        equ 2       ; 下方向
    LEFT        equ 3       ; 左方向
    RIGHT       equ 4       ; 右方向
    BLOCK_SIZE  equ 25      ; 位图方块大小
    MAP_HEIGHT  equ 30      ; 画面高度，30个位图
    MAP_WIDTH   equ 30      ; 画面宽度，30个位图
    BLOCK_NUM   equ 900     ; 总位图个数

    appName         byte "Grady Snake", 0           ; 主窗口名
    appClassName    byte "GradySnake", 0            ; 主窗口类名
    scoreFormat     byte "Score: %d rank1:%s ",0             ; 分数输出格式
    MessageBoxTitle byte "Result", 0                ; MessageBox标题
    MessageBoxFormat byte "You died! Score: %d", 0  ; MessageBox输出格式
    MessageBoxBuffer byte 64 dup(?)                 ; MessageBox输出

    tmp           DWORD BLOCK_NUM dup(-1)           ; 临时缓存区，用于存储可用的坐标（不与图上已有点冲突）

    snakeX        dd BLOCK_NUM dup(?)               ; 蛇的x坐标数组, snakeX[0]表示蛇头部的x坐标
    snakeY        dd BLOCK_NUM dup(?)               ; 蛇的y坐标数组，snakeY[0]表示蛇头部的y坐标
    snakeDir      dd ?                              ; 蛇头的方向
    snakeSize     dd ?                              ; 蛇的长度

    appleX        DWORD -1                          ; 食物在图上的x坐标，-1表示不存在
    appleY        DWORD -1                          ; 食物在图上的y坐标，-1表示不存在
    heartX        DWORD -1                          ; 爱心在图上的x坐标，-1表示不存在
    heartY        DWORD -1                          ; 爱心在图上的y坐标，-1表示不存在
    starX         DWORD -1                          ; 星星在图上的x坐标，-1表示不存在
    starY         DWORD -1                          ; 星星在图上的y坐标，-1表示不存在
    boomX         DWORD -1                          ; 炸弹在图上的x坐标，-1表示不存在
    boomY         DWORD -1                          ; 炸弹在图上的y坐标，-1表示不存在
    snakeHandle     dd ?                            ; 蛇位图handle
    appleHandle     dd ?                            ; 食物位图handle
    starHandle      dd ?                            ; 星星位图handle
    heartHandle     dd ?                            ; 爱心位图handle
    boomHandle      dd ?                            ; 炸弹位图handle

    score           dd ?                            ; 游戏分数
    
    ; 随机数变量
    prng_x        dd 0
    prng_a        dd 100711433

    ; Windows窗口handle
    CommandLine   dd 0
    hWnd          dd 0
    hInstance     dd 0
    hCompatibleDC dd ?
    ThreadID      DWORD   ?
    hEventStart   HANDLE  ?
    dwExitCode    LPDWORD ?

    ; 分数文本框
    textColor     dd 0F0F0F0h
    text          dw 64 dup(?)
    textRect      RECT <>

	; 历史记录相关 hsj
	file1_path DB 'rank.txt', 0 
	buffer1 byte 2048 dup(0) 
	rank byte 1024*50 dup(0) 
	rankContent db '第%d级需得分%s', 0AH,0
	ranktell db'您为第%d级',0AH,0
	tell byte 100 dup(0) 
	maxone  byte 2048 dup(0)
	buffer_score byte 2048 dup(0) 
	szmsg byte "HELLO",0ah,0
	szmsg2 byte "world",0ah,0
	MessagerankTitle byte "排名", 0  
	len1 dword 0 
	;hsj
.code

start:
    invoke GetModuleHandle, NULL
    mov hInstance, eax
    invoke GetCommandLine
    mov CommandLine, eax
    invoke WinMain, hInstance, NULL, CommandLine, SW_SHOWDEFAULT
    invoke ExitProcess, eax
;hsj
compare proc str1:ptr byte,str2:ptr byte
	mov esi,   str1  
	mov edi ,  str2 
L0:	
	mov  al,byte ptr[esi]
	mov  dl,byte ptr[edi]
	.IF  al>dl
	mov eax,1
	ret
	.ENDIF
	.IF  al<dl
	mov eax,-1
	ret
	.ENDIF
	inc esi
	inc edi
	cmp byte ptr[esi],0
	je endfunc
	cmp byte ptr[edi],0
	je endfunc
	jmp L0
endfunc:
	mov eax,0
	ret
compare endp

dtoc proc   
    mov eax,score
	xor edx,edx
	xor ecx,ecx
	mov ebx, 10	;设置除数

	rem:	
	div ebx	;执行安全的除法
	push edx
	inc ecx
	xor edx,edx
	cmp eax,edx
	jnz rem
	mov edi,offset buffer_score
		
	copy:	;把栈中的数据复制到string中
	pop eax		
	add al,'0';把对应的数字转换成ASCII码 
	mov [edi],al
	inc edi
	loop copy  
	mov byte ptr[edi],0 
	ret
dtoc endp

;hsj
GetLine proc fp: HANDLE, buffer2:ptr byte, worktype:DWORD;读取rank文件的每一个行，文件内已排好序
	local len: dword
	local char: byte 
	push esi
	mov esi, buffer2
	mov edi, 0  

L0:	invoke ReadFile, fp, addr char, 1, addr len, NULL
	cmp len, 0
	je L1 
	cmp char, 10
	je L1
	mov al, char
	mov byte ptr [esi], al
	inc esi
	inc edi 
	jmp L0

L1:
   .IF worktype==0
	dec esi
	.ENDIF 
	.IF worktype==2
	dec esi
	.ENDIF 
	mov byte ptr [esi], 0  
	mov eax,edi
	pop esi
	ret 

GetLine endp
;hsj
Fileprocess proc fpath1:ptr byte, worktype:dword
	
	local line1 :dword 
	local file1 :HANDLE 
	local index_line :dword   
	local buffer_ranke[1024] :byte 
	mov index_line, 0  
	mov esi, offset rank
	mov byte ptr[esi], 0
	invoke CreateFile, fpath1, GENERIC_READ, FILE_SHARE_READ, NULL,OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
	mov file1, eax 
	invoke dtoc  
L0:
	inc index_line 
	invoke GetLine, file1, offset buffer1, worktype
	mov line1, eax  
	
L1:
	cmp line1, 0
	jne L2   
	jmp ENDFUNC
L2:
	.IF worktype==0
	.IF index_line==1 
	invoke strcpy, offset maxone, addr buffer1
	;invoke printf,offset maxone
	jmp ENDFUNC
	.ENDIF
	.ENDIF
	.IF worktype==2   
		invoke strlen, offset buffer_score     
		mov len1, eax 
		invoke strlen, offset buffer1  
		 .IF eax<len1   
		 invoke sprintf, addr buffer_ranke, offset ranktell, index_line 
		 ;invoke printf,addr buffer_ranke
		 invoke strcat, offset tell, addr buffer_ranke 
		 mov worktype,-1 
		 ret 
		 .ELSEIF eax==len1 
		 invoke compare,offset buffer_score,offset buffer1 
		 .IF eax==1 
		 invoke sprintf, addr buffer_ranke, offset ranktell, index_line  
		 ;invoke printf,addr buffer_ranke
		 invoke strcat, offset tell, addr buffer_ranke 
		 mov worktype,-1 
		 ret
		 .ENDIF
		 .ENDIF 
	.ENDIF
	invoke sprintf, addr buffer_ranke, offset rankContent, index_line,offset buffer1
	;invoke printf,addr buffer_ranke
	invoke strcat, offset rank, addr buffer_ranke 
	jmp L0 
	

ENDFUNC: 
	ret 
Fileprocess endp
;hsj

WinMain proc hInst:DWORD, hPrevInst:DWORD, CmdLine:DWORD, CmdShow:DWORD
    LOCAL wc:WNDCLASSEX
    LOCAL msg:MSG

    LOCAL Wwd:DWORD
    LOCAL Wht:DWORD
    LOCAL Wtx:DWORD
    LOCAL Wty:DWORD
	
    mov wc.cbSize, sizeof WNDCLASSEX
    mov wc.style, CS_HREDRAW or CS_VREDRAW or CS_BYTEALIGNWINDOW
	invoke Fileprocess,offset file1_path, 0 
    mov wc.lpfnWndProc, offset WndProc
    mov wc.cbClsExtra, NULL
    mov wc.cbWndExtra, NULL
    push hInst
    pop wc.hInstance
    mov wc.hbrBackground, COLOR_BTNFACE+1 
    mov wc.lpszMenuName, NULL
    mov wc.lpszClassName, offset appClassName
    invoke LoadIcon, hInst, 500
    mov wc.hIcon, eax
    invoke LoadCursor, NULL, IDC_ARROW
    mov wc.hCursor, eax
    mov wc.hIconSm, 0

    invoke RegisterClassEx, ADDR wc

    mov Wwd, BLOCK_SIZE * MAP_WIDTH + 10
    mov Wht, BLOCK_SIZE * MAP_HEIGHT + 32
    
    invoke GetSystemMetrics, SM_CXSCREEN
    invoke TopXY, Wwd, eax
    mov Wtx, eax

    invoke GetSystemMetrics, SM_CYSCREEN
    invoke TopXY, Wht, eax
    mov Wty, eax

    invoke CreateWindowEx, WS_EX_OVERLAPPEDWINDOW,\
                        ADDR appClassName,\
                        ADDR appName,\
                        WS_SYSMENU,\
                        Wtx,\
                        Wty,\
                        Wwd,\
                        Wht,\
                        NULL,
                        NULL,\
                        hInst,\
                        NULL
                            
    mov hWnd, eax

    invoke ShowWindow, hWnd, SW_SHOWNORMAL
    invoke UpdateWindow, hWnd

    StartLoop:
        invoke GetMessage, ADDR msg, NULL, 0, 0
        cmp eax, 0
        je ExitLoop
        invoke TranslateMessage, ADDR msg
        invoke DispatchMessage, ADDR msg
        jmp StartLoop
    ExitLoop:

    mov eax, msg.wParam
    ret
WinMain endp



WndProc proc hWin:DWORD, uMsg:DWORD, wParam:DWORD, lParam:DWORD

LOCAL DCHandle:DWORD
LOCAL paint:PAINTSTRUCT

.IF uMsg == WM_CREATE

        ; 加载蛇位图
        invoke LoadBitmap, hInstance, SNAKE
        mov    snakeHandle, eax
        ; 加载食物位图
        invoke LoadBitmap, hInstance, APPLE
        mov    appleHandle, eax
        ; 加载星星位图
        invoke LoadBitmap, hInstance, STAR
        mov starHandle, eax
        ; 加载爱心位图
        invoke LoadBitmap, hInstance, HEART
        mov heartHandle, eax
        ; 加载炸弹位图
        invoke LoadBitmap, hInstance, BOOM
        mov boomHandle, eax
        ; 初始化游戏参数
        invoke initGame
        ; 初始化进程
        invoke CreateEvent, NULL, FALSE, FALSE, NULL
        mov    hEventStart, eax
        mov    eax, OFFSET ThreadProc
        invoke CreateThread, NULL, NULL, eax, NULL, NORMAL_PRIORITY_CLASS, ADDR ThreadID

.ELSEIF uMsg == WM_DESTROY
        ; 关闭窗口，关闭handle
        invoke DeleteObject, appleHandle
        invoke DeleteObject, snakeHandle
        invoke DeleteObject, starHandle
        invoke DeleteObject, boomHandle
        invoke DeleteObject, heartHandle
        invoke PostQuitMessage,NULL
        mov eax, 0
        ret

.ELSEIF uMsg == WM_KEYDOWN
        ; 键盘输入
        ; 当蛇长度为一时可以任意移动
        .IF snakeSize == 1
            .IF wParam == VK_UP
                mov snakeDir, UP
            .ELSEIF wParam == VK_RIGHT
                mov snakeDir, RIGHT
            .ELSEIF wParam == VK_DOWN
                mov snakeDir, DOWN
            .ELSEIF wParam == VK_LEFT
                mov snakeDir, LEFT
            .ENDIF
        .ENDIF

        ; 当蛇长度大于一时，不能朝当前移动方向的反方向移动
        ; 如：当前向左移动，不能立刻向右移动，只能向上，向下移动后再向右移动
        .IF snakeSize > 1
            .IF wParam == VK_UP && snakeDir != DOWN
                mov snakeDir, UP
            .ELSEIF wParam == VK_DOWN && snakeDir != UP
                mov snakeDir, DOWN
            .ELSEIF wParam == VK_LEFT && snakeDir != RIGHT
                mov snakeDir, LEFT
            .ELSEIF wParam == VK_RIGHT && snakeDir != LEFT
                mov snakeDir, RIGHT
            .ENDIF
        .ENDIF
.ELSEIF uMsg == WM_FINISH
        invoke  InvalidateRect, hWnd, NULL, TRUE

.ELSEIF uMsg == WM_PAINT
        ; 绘制画布
        invoke BeginPaint, hWin, ADDR paint
        mov    DCHandle, eax
        invoke CreateCompatibleDC, DCHandle
        mov    hCompatibleDC, eax

        ; 绘制食物
        invoke SelectObject, hCompatibleDC, appleHandle
        mov  ebx, appleX
        imul ebx, BLOCK_SIZE    ; 转成真实的像素坐标，第几个block乘上block size
        mov  ecx, appleY
        imul ecx, BLOCK_SIZE    ; 转成真实的像素坐标，第几个block乘上block size
        invoke BitBlt, DCHandle, ebx, ecx, BLOCK_SIZE, BLOCK_SIZE, hCompatibleDC, 0, 0, SRCCOPY

        ; 如果当前有星星，则绘制星星，绘制方法同食物
        .IF starX != -1
            invoke SelectObject, hCompatibleDC, starHandle
            mov ebx, starX
            imul ebx, BLOCK_SIZE
            mov ecx, starY
            imul ecx, BLOCK_SIZE
            invoke BitBlt, DCHandle, ebx, ecx, BLOCK_SIZE, BLOCK_SIZE, hCompatibleDC, 0, 0, SRCCOPY
        .ENDIF

        ; 如果当前有爱心，则绘制爱心，绘制方法同食物
        .IF heartX != -1
            invoke SelectObject, hCompatibleDC, heartHandle
            mov ebx, heartX
            imul ebx, BLOCK_SIZE
            mov ecx, heartY
            imul ecx, BLOCK_SIZE
            invoke BitBlt, DCHandle, ebx, ecx, BLOCK_SIZE, BLOCK_SIZE, hCompatibleDC, 0, 0, SRCCOPY
        .ENDIF

        ; 如果当前有炸弹，则绘制炸弹，绘制方法同食物
        .IF boomX != -1
            invoke SelectObject, hCompatibleDC, boomHandle
            mov ebx, boomX
            imul ebx, BLOCK_SIZE
            mov ecx, boomY
            imul ecx, BLOCK_SIZE
            invoke BitBlt, DCHandle, ebx, ecx, BLOCK_SIZE, BLOCK_SIZE, hCompatibleDC, 0, 0, SRCCOPY
        .ENDIF

        ;绘制蛇
        invoke SelectObject, hCompatibleDC, snakeHandle
        mov edi, 0
        drawSnake:
            mov ebx, snakeX[4 * edi]

            cmp ebx, -1
            je drawFinish

            mov ebx, snakeX[4 * edi]
            imul ebx, BLOCK_SIZE

            mov  ecx, snakeY[4 * edi]
            imul ecx, BLOCK_SIZE

            invoke BitBlt, DCHandle, ebx, ecx, BLOCK_SIZE, BLOCK_SIZE, hCompatibleDC, 0, 0, SRCCOPY
            
            inc edi
            jmp drawSnake
        drawFinish:

        ; 绘制分数栏目
        mov eax, score
		
        invoke crt_sprintf, addr text, addr scoreFormat, score, offset maxone;hsj
        ; 分数栏位置 
        mov textRect.left, 10
        mov textRect.top, 10
        mov textRect.right, 220;hsj
        mov textRect.bottom, 30
        ; 绘制
        invoke SetBkColor, DCHandle, textColor
        invoke DrawText, DCHandle, ADDR text, -1, ADDR textRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER 
        invoke DeleteDC, hCompatibleDC
        invoke EndPaint, hWin, ADDR paint

        mov eax, 0
        ret

.ENDIF

    invoke DefWindowProc,hWin,uMsg,wParam,lParam
    ret

WndProc endp


TopXY proc wDim:DWORD, sDim:DWORD
    shr sDim, 1
    shr wDim, 1
    mov eax, wDim
    sub sDim, eax

    mov eax, sDim
    ret
TopXY endp

ThreadProc PROC USES ecx Param:DWORD
LOCAL i:DWORD
    
    invoke WaitForSingleObject, hEventStart, 100

    .IF eax == WAIT_TIMEOUT

        mov esi, snakeSize
        dec esi
        
        ; 将蛇向前移动一个位置
        ; 如当前蛇的数组[(x1, y1), (x2, y2), (x3, y3)]
        ; 变成：[(x1, y1), (x1, y1), (x2, y2)]
        movForward:
            mov ebx, snakeX[4 * esi]
            mov snakeX[4 * esi + 4], ebx
            mov ebx, snakeY[4 * esi]
            mov snakeY[4 * esi + 4], ebx
            cmp esi, 0
            jz movFinish
            dec esi
            jmp movForward
        movFinish:

        ; 根据当前全局方向更新蛇数组的第一个位置
        ; 即把上述[(x1, y1), (x1, y1), (x2, y2)]
        ; 变成[(x_new, y_new), (x1, y1), (x2, y2)]
        .IF snakeDir == UP
            sub snakeY[0], 1
        .ELSEIF snakeDir == RIGHT
            add snakeX[0], 1
        .ELSEIF snakeDir == DOWN
            add snakeY[0], 1
        .ELSEIF snakeDir == LEFT
            sub snakeX[0], 1
        .ENDIF

        ; 检查蛇是否吃掉食物
        mov eax, appleX
        mov ebx, appleY
        .IF eax == snakeX && ebx == snakeY
            invoke randomApple  ; 吃到食物加10分，变长一个单位
            add score, 10
            inc snakeSize
        .ENDIF 

        ; 检查蛇是否吃到星星
        mov eax, starX
        mov ebx, starY
        .IF eax == snakeX && ebx == snakeY
            add score, 50       ; 吃到星星加50分，不变长度
            mov starX, -1
            mov starY, -1
        .ENDIF

        ; 检查蛇是否吃到爱心
        mov eax, heartX
        mov ebx, heartY
        .IF eax == snakeX && ebx == snakeY
            add score, 0
            .IF snakeSize > 1
                dec snakeSize   ; 吃到爱心变短一个单位
            .ENDIF
            mov heartX, -1
            mov heartY, -1
        .ENDIF

        ; 检查蛇是否吃到炸弹
        mov eax, boomX
        mov ebx, boomY
        .IF eax == snakeX && ebx == snakeY
            sub score, 50       ; 吃到炸弹减50分
            add snakeSize, 5    ; 吃到炸弹变长5个单位
            mov boomX, -1
            mov boomY, -1
        .ENDIF

        ; 更新完长度后更新蛇的数组结束位置
        mov ebx, snakeSize
        mov snakeX[4 * ebx], -1
        mov snakeY[4 * ebx], -1

        ; 检查是否与边界碰撞
        .IF snakeX[0] < 0 || snakeY[0] < 0 || snakeX[0] >= MAP_WIDTH || snakeY[0] >= MAP_HEIGHT
            invoke crt_sprintf, addr MessageBoxBuffer, addr MessageBoxFormat, score
			invoke MessageBoxA, NULL, offset MessageBoxBuffer, offset MessageBoxTitle, MB_OK 
			invoke Fileprocess,offset file1_path, 1
			invoke MessageBoxA, NULL, offset rank, offset MessagerankTitle, MB_OK  ;HSJ
			invoke Fileprocess,offset file1_path, 2
			invoke MessageBoxA, NULL, offset tell, offset MessagerankTitle, MB_OK  ;HSJ
			invoke initGame
        .ENDIF

        ; 检查是否撞到自己
        mov esi, 1
        .WHILE esi < snakeSize
            mov eax, snakeX[4 * esi]
            mov ebx, snakeY[4 * esi]
            .IF snakeX[0] == eax && snakeY[0] == ebx
                invoke crt_sprintf, addr MessageBoxBuffer, addr MessageBoxFormat, score
				invoke Fileprocess,offset file1_path, 1
				invoke MessageBoxA, NULL, offset rank, offset MessagerankTitle, MB_OK  ;hsj
                invoke MessageBoxA, NULL, offset MessageBoxBuffer, offset MessageBoxTitle, MB_OK
				invoke Fileprocess,offset file1_path, 2
				invoke MessageBoxA, NULL, offset tell, offset MessagerankTitle, MB_OK  ;HSJ
                invoke initGame
            .ENDIF
            inc esi
        .ENDW

        invoke  SendMessage, hWnd, WM_FINISH, NULL, NULL

    .ENDIF
    
    jmp   ThreadProc
    ret
ThreadProc endp


initGame proc

    ; 道具初始化
    mov starX, -1
    mov starY, -1
    mov heartX, -1
    mov heartY, -1
    mov boomX, -1
    mov boomY, -1
    mov score, 0
    
    ; 蛇长度初始化
    mov snakeSize, 1

    ; 蛇方向初始化
    invoke random, 4
    mov snakeDir, eax

    ; 蛇位置初始化
    invoke random , MAP_WIDTH/2 
	add eax,MAP_WIDTH/4 
    mov snakeX[0], eax
    invoke random , MAP_HEIGHT/2
	add eax,MAP_HEIGHT/4 
    mov snakeY[0], eax

    ; 蛇数组初始化
    mov ecx,1
    .WHILE ecx < BLOCK_NUM
        mov snakeX[4 * ecx], -1
        mov snakeY[4 * ecx], -1 
        inc ecx
    .ENDW

    ; 随机初始化食物坐标
    invoke randomApple
    ret
initGame endp

random proc range:DWORD   
    rdtsc
    adc eax, edx
    adc eax, prng_x
    mul prng_a
    adc eax, edx
    mov prng_x, eax
    mul range
    mov eax, edx
    ret
random endp

randomApple proc
    ; 初始化食物坐标，更新在appleX，appleY全局变量中
    LOCAL tot:DWORD

    mov esi, 0
    mov tot, 0
    
    ; 挨个遍历位图
    .WHILE esi < BLOCK_NUM

        ; 挨个遍历蛇位置
        mov edi, 0
        .WHILE edi < snakeSize
            ; 二维坐标转一维坐标
            mov eax, snakeY[4 * edi]
            imul eax, MAP_WIDTH
            add eax, snakeX[4 * edi]

            .IF esi == eax
                jmp confict
            .ENDIF

            .IF starX != -1 && starY != -1
                mov eax, starY
                imul eax, MAP_WIDTH
                add eax, starX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF

            .IF heartX != -1 && heartY != -1
                mov eax, heartX
                imul eax, MAP_WIDTH
                add eax, heartY
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF
        
            inc edi
        .ENDW

        ; 当前枚举位置没有冲突，更新到tmp数组中
        mov edx, esi
        mov ecx, tot
        mov tmp[4 * ecx], edx
        inc tot

    confict:
        inc esi

    .ENDW

    dec tot
    invoke random, tot

    mov eax, tmp[4 * eax]
    mov ebx, MAP_WIDTH
    mov edx, 0
    div ebx
    ; 一维坐标转二维坐标
    mov appleX, edx
    mov appleY, eax

    ; 按概率创建道具
    mov eax, 5
    invoke random, eax
    .IF eax == 0
        invoke randomStar
    .ENDIF
    .IF eax <= 1
        invoke randomHeart
    .ENDIF
    .IF eax <= 2
        invoke randomBoom
    .ENDIF
    ret
randomApple endp


randomStar proc
LOCAL tot:DWORD
    ; 随机初始化星星位置，同上
    mov esi, 0
    mov tot, 0
    
    .WHILE esi < BLOCK_NUM
        mov edi, 0

        .WHILE edi < snakeSize
            
            mov eax, snakeY[4 * edi]
            imul eax, MAP_WIDTH
            add eax, snakeX[4 * edi]

            .IF esi == eax
                jmp confict
            .ENDIF

            .IF appleX != -1 && appleY != -1
                mov eax, appleY
                imul eax, MAP_WIDTH
                add eax, appleX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF

            .IF heartX != -1 && heartY != -1
                mov eax, heartX
                imul eax, MAP_WIDTH
                add eax, heartY
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF
        
            inc edi
        .ENDW

        mov edx, esi
        mov ecx, tot
        mov tmp[4 * ecx], edx
        inc tot

    confict:
        inc esi

    .ENDW

    dec tot
    invoke random, tot

    mov eax, tmp[4 * eax]
    mov ebx, MAP_WIDTH
    mov edx, 0
    div ebx

    mov starX, edx
    mov starY, eax

    ret

randomStar endp


randomHeart proc
LOCAL tot:DWORD
    ; 随机初始化爱心位置，同上
    mov esi, 0
    mov tot, 0
    
    .WHILE esi < BLOCK_NUM
        mov edi, 0

        .WHILE edi < snakeSize
            
            mov eax, snakeY[4 * edi]
            imul eax, MAP_WIDTH
            add eax, snakeX[4 * edi]

            .IF esi == eax
                jmp confict
            .ENDIF

            .IF appleX != -1 && appleY != -1
                mov eax, appleY
                imul eax, MAP_WIDTH
                add eax, appleX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF

            .IF starX != -1 && starY != -1
                mov eax, starY
                imul eax, MAP_WIDTH
                add eax, starX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF
        
            inc edi
        .ENDW

        mov edx, esi
        mov ecx, tot
        mov tmp[4 * ecx], edx
        inc tot

    confict:
        inc esi

    .ENDW

    dec tot
    invoke random, tot

    mov eax, tmp[4 * eax]
    mov ebx, MAP_WIDTH
    mov edx, 0
    div ebx

    mov heartX, edx
    mov heartY, eax

    ret

randomHeart endp

randomBoom proc
LOCAL tot:DWORD
    ; 随机初始化炸弹位置，同上
    mov esi, 0
    mov tot, 0
    
    .WHILE esi < BLOCK_NUM
        mov edi, 0

        .WHILE edi < snakeSize
            
            mov eax, snakeY[4 * edi]
            imul eax, MAP_WIDTH
            add eax, snakeX[4 * edi]

            .IF esi == eax
                jmp confict
            .ENDIF

            .IF appleX != -1 && appleY != -1
                mov eax, appleY
                imul eax, MAP_WIDTH
                add eax, appleX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF

            .IF starX != -1 && starY != -1
                mov eax, starY
                imul eax, MAP_WIDTH
                add eax, starX
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF

            .IF heartX != -1 && heartY != -1
                mov eax, heartX
                imul eax, MAP_WIDTH
                add eax, heartY
                .IF esi == eax
                    jmp confict
                .ENDIF
            .ENDIF
        
            inc edi
        .ENDW

        mov edx, esi
        mov ecx, tot
        mov tmp[4 * ecx], edx
        inc tot

    confict:
        inc esi

    .ENDW

    dec tot
    invoke random, tot

    mov eax, tmp[4 * eax]
    mov ebx, MAP_WIDTH
    mov edx, 0
    div ebx

    mov boomX, edx
    mov boomY, eax

    ret

randomBoom endp

end start

