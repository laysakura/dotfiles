; ウィンドウ: 最大化/最小化
PrintScreen & f::
  WinGet, st, MinMax, A
  If st = 1
    WinRestore, A
  Else
    WinMaximize, A
return

; ウィンドウ: 閉じる
PrintScreen & c::WinClose, A

; 仮想デスクトップ: 次
#n::Send,#^{Right}

; 仮想デスクトップ: 前
#p::Send,#^{Left}

; 仮想デスクトップ: 一覧
#i::Send,#{Tab}

; アプリケーション: MSYS2 Shell
#Enter::Run,"C:\msys64\msys2_shell.cmd"
