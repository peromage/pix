### init.ps1 -- PowerShell bootstrap

### Prerequisites
## PowerShell 7 is required
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host -ForegroundColor Red "PowerShell 7 and above required"
    return
}

## Global variables
$RICE = @{}
$RICE.rc = Get-Item "$PSScriptRoot"
$RICE.custom_rc = (Join-Path $RICE.rc "custom.ps1")
$RICE.privileged = $IsWindows ? ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) : ((id -u) -eq 0)

## Return a string with dot sourcing syntax
## Due to the limitation, sourcing has to be done in the global scope
function rice_include_expr {
    param ($file)
    return ". $(Join-Path $RICE.rc.FullName librice $file) $args"
}

### Loading modules
&{
    rice_include_expr alias.ps1
    rice_include_expr prompts/minimalist.ps1
    if ($IsWindows) {
        rice_include_expr alias-windows.ps1
    }
} | ForEach-Object { Invoke-Expression $_ }

### PSReadLine
&{
    $my_psreadline_options = @{
        EditMode = "Emacs"
        HistoryNoDuplicates = $true
        HistorySearchCursorMovesToEnd = $true
        HistorySearchCaseSensitive = $false
        HistorySaveStyle = "SaveIncrementally"
        PredictionSource = "History"
    }
    Set-PSReadLineOption @my_psreadline_options
    Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
}

### Random stuff starts here
if (Test-Path -Type Leaf $RICE.custom_rc) { . $RICE.custom_rc }