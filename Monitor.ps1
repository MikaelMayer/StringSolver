# File monitoring:
#[System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") 
[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")
[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Timers")
[void] [System.Reflection.assembly]::loadwithpartialname("System.Drawing")
  
# Unregister all other events
Get-EventSubscriber | Unregister-Event

$folder = 'C:\Users\Mikael\Dropbox\workspace\StringSolver' # Enter the root path you want to monitor. 
$filter = '*' # You can enter a wildcard filter here.

<# This is probably not the best code I've ever written, but
   I think it should be readable for most (advanced) users.

   I will wrap this function into a Cmdlet when I have time to do it.
   Feel free to edit this answer and improve it!
#>


# In the following line, you can change 'IncludeSubdirectories to $true if required. 
$fsw = New-Object IO.FileSystemWatcher $folder, $filter -Property @{
  IncludeSubdirectories = $true;
  NotifyFilter = [System.IO.NotifyFilters]'FileName, LastWrite'}
$fsw.EnableRaisingEvents = $true

$AutoAction = {
	"Event fired" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
	$ie = New-Object -com InternetExplorer.Application
	$ie.navigate2("https://intranet.mydomain.com/")
	$ie.visible = $true
}

$RenamedAction = {
  function getRelativePath([string]$from, [string]$to) {
    return python -c "import os.path; print os.path.relpath('$to', '$from')" ;
  }

  $tmpwdir = Get-Location
  #if ($enableEvent -eq $true) {
  $oldabspath = $Event.SourceEventArgs.OldFullPath
  $newabspath = $Event.SourceEventArgs.FullPath
  $wdir = Split-Path -parent $oldabspath
  Set-Location $wdir
  
  "Started writing... $wdir $oldabspath, $newabspath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  $oldpath = getRelativePath -From $wdir -To $oldabspath
  $newpath = getRelativePath -From $wdir -To $newabspath
  
  "old abs path: $oldabspath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  "old rel path: $oldpath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  "Java command:" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  
  "java -jar ""c:/Users/Mikael/Dropbox/workspace/StringSolver/target/scala-2.10/stringsolver_2.10-1.0-one-jar.jar"" move --mintwoexamples --explain --test $oldpath $newpath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  $out = java -jar "c:/Users/Mikael/Dropbox/workspace/StringSolver/target/scala-2.10/stringsolver_2.10-1.0-one-jar.jar" move --mintwoexamples --explain --test $oldpath $newpath 2>&1 | Out-String # | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  $out | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  "Finished writing" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append

  $OUTPUT = "NO" #[System.Windows.Forms.MessageBox]::Show("Do you want to rename the files according to the following pattern:`r`n $out ?" , "File renaming suggestion" , 4)
  Set-Location $tmpwdir
  
  if($out.Contains("->")) {
	"Out event found" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
    $title = "File renaming suggestion" 
	$msg = "Rename?`r`n $out" 
	$timeout = 10000
	$icon = [System.Windows.Forms.ToolTipIcon]::Info
	$Balloon = new-object System.Windows.Forms.NotifyIcon  
	$Balloon.Icon = [System.Drawing.SystemIcons]::Question
	$Balloon.Visible = $true; 
	Unregister-Event -SourceIdentifier click_event -ErrorAction SilentlyContinue 
    Register-ObjectEvent -InputObject $Balloon -EventName BalloonTipClicked -sourceIdentifier click_event -Action $AutoAction
    $Balloon.ShowBalloonTip($timeout, $title, $msg, $icon); 
	
	#sleep(1)
    "Waiting for click event" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
    wait-event -timeout 10 -sourceIdentifier click_event > $null
    Remove-Event click_event -ea SilentlyContinue
	#sleep(1000)
    #"Event click removed" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
		
#{ 
#	"Going to perform the global change" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
#    $fsw.EnableRaisingEvents = $false
#	Set-Location $wdir
#	java -jar "c:/Users/Mikael/Dropbox/workspace/StringSolver/target/scala-2.10/stringsolver_2.10-1.0-one-jar.jar" move --mintwoexamples --all
#	$fsw.EnableRaisingEvents = $true
#	Set-Location $tmpwdir
#	"Going back to reality" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
#	} | Out-Null 
  }
  "Back to $tmpwdir `r`n Script finished" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  Set-Location $tmpwdir
}


# See http://msdn.microsoft.com/en-us/library/system.io.filesystemwatcher.renamed(v=vs.110).aspx?cs-save-lang=1&cs-lang=vb#code-snippet-2
Register-ObjectEvent -InputObject $fsw -EventName Renamed -SourceIdentifier FileRenamed1 -Action $RenamedAction
Get-EventSubscriber
# To stop the monitoring, run the following commands: 
# Unregister-Event FileRenamed1


