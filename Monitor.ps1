# Name  :   Monitor.ps1
# Org   :   LARA - MPI
# Date  :   11.02.2014
# Author:   Mikaël Mayer
# Function: Semi-automatic File renaming Monitoring as Windows
#           Run it after running "Powershell -sta" on Powershell
#           To stop monitoring event, run:
#           "Get-EventSubscriber | Unregister-Event"

# Enter the root path you want to monitor. 
$folder = 'C:\Users\Mikael\Dropbox\workspace\StringSolver' 
# Enter the location of the StringSolver-jar
$stringsolver = "c:/Users/Mikael/Dropbox/workspace/StringSolver/target/scala-2.10/stringsolver_2.10-1.0-one-jar.jar"



#[System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms") 
[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")
[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Timers")
[void] [System.Reflection.assembly]::loadwithpartialname("System.Drawing")
  
# Unregister all other events
Get-EventSubscriber | Unregister-Event


$filter = '*' # You can enter a wildcard filter here.
# In the following line, you can change 'IncludeSubdirectories to $true if required. 
$fsw = New-Object IO.FileSystemWatcher $folder, $filter -Property @{
  IncludeSubdirectories = $true;
  NotifyFilter = [System.IO.NotifyFilters]'FileName, LastWrite'}
$fsw.EnableRaisingEvents = $true

	
$global:workingdir = "Global value of workingdir"
$global:tmpworkingdir = $null
$global:watchingsystem = $null
$global:balloon = $null
Set-Variable -Name watchingsystem -Value $fsw -Scope Global

$RenamedAction = {
  function getRelativePath([string]$from, [string]$to) {
    $pathto = $to -replace "\\", "/"
    $pathfrom = $from -replace "\\", "/"
    return python -c "import os.path; print os.path.relpath('$pathto', '$pathfrom')" ;
  }

  $tmpwdir = Get-Location
  #if ($enableEvent -eq $true) {
  $oldabspath = $Event.SourceEventArgs.OldFullPath
  $newabspath = $Event.SourceEventArgs.FullPath
  $wdir = Split-Path -parent $oldabspath
  Set-Location $wdir
  
  # "Started writing... $wdir $oldabspath, $newabspath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  $oldpath = getRelativePath -From $wdir -To $oldabspath
  $newpath = getRelativePath -From $wdir -To $newabspath
  
  # "old abs path: $oldabspath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  # "old rel path: $oldpath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  # "Java command:" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  
  # "java -jar ""$stringsolver"" move --mintwoexamples --explain --test $oldpath $newpath" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  $out = java -jar $stringsolver move --mintwoexamples --explain --test $oldpath $newpath 2>&1 | Out-String # | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  # $out | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  # "Finished writing" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append

  $OUTPUT = "NO" #[System.Windows.Forms.MessageBox]::Show("Do you want to rename the files according to the following pattern:`r`n $out ?" , "File renaming suggestion" , 4)
  Set-Location $tmpwdir
  
  if($out.Contains("->")) {
	# "Out event found" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
    $title = "File renaming suggestion" 
	$msg = "Rename?`r`n $out" 
	$timeout = 10000
	$icon = [System.Windows.Forms.ToolTipIcon]::Info
	$Balloon = new-object System.Windows.Forms.NotifyIcon  
	$Balloon.Icon = [System.Drawing.SystemIcons]::Question
	$Balloon.Visible = $true; 
	Unregister-Event -SourceIdentifier click_event -ErrorAction SilentlyContinue 
	Set-Variable -Name workingdir -Value $wdir -Scope Global
	Set-Variable -Name tmpworkingdir -Value $tmpwdir -Scope Global
	Set-Variable -Name balloon -Value $Balloon -Scope Global

    Register-ObjectEvent -InputObject $Balloon -EventName BalloonTipClicked -sourceIdentifier click_event `
	-Action {
	    $tmpwdir = $global:tmpworkingdir
	    $wdir = $global:workingdir
	    $fsw = $global:watchingsystem

        #[System.Windows.Forms.MessageBox]::Show("Going to perform the mapping in $wdir with $tmpwdir","Info - $fsw");
	    Set-Location $wdir;
		$fsw.EnableRaisingEvents = $false;
     	java -jar $stringsolver move --mintwoexamples --all;
     	$fsw.EnableRaisingEvents = $true;
    	Set-Location $tmpwdir;
		$balloon.dispose()
		#[System.Windows.Forms.MessageBox]::Show("java -jar ""c:/Users/Mikael/Dropbox/workspace/StringSolver/target/scala-2.10/stringsolver_2.10-1.0-one-jar.jar"" move --mintwoexamples --all (in $wdir )","Mapping done");
   }.GetNewClosure() 

    $Balloon.ShowBalloonTip($timeout, $title, $msg, $icon); 
	#$Balloon.dispose()
	#sleep(1)
    # "Waiting for click event" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
    wait-event -timeout 1 -sourceIdentifier click_event > $null
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
  # "Back to $tmpwdir `r`n Script finished" | Out-File C:\Users\Mikael\Dropbox\workspace\StringSolver\logrename.txt -Append
  Set-Location $tmpwdir
}


# See http://msdn.microsoft.com/en-us/library/system.io.filesystemwatcher.renamed(v=vs.110).aspx?cs-save-lang=1&cs-lang=vb#code-snippet-2
Register-ObjectEvent -InputObject $fsw -EventName Renamed -SourceIdentifier FileRenamed1 -Action $RenamedAction
Get-EventSubscriber
# To stop the monitoring, run the following commands: 
# Unregister-Event FileRenamed1


