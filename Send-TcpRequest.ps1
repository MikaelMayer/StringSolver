############################################################################## 
## Send-TcpRequest.ps1 
## 
## From Windows PowerShell Cookbook (O’Reilly) 
## by Lee Holmes (http://www.leeholmes.com/guide) 
## 
## Send a TCP request to a remote computer, and return the response. 
## If you do not supply input to this script (via either the pipeline, or the 
## -InputObject parameter,) the script operates in interactive mode. 
## 
## Example: 
## 
## $http = @" 
## GET / HTTP/1.1 
## Host:search.msn.com 
## `n`n 
## "@ 
## 
## $http | Send-TcpRequest search.msn.com 80 
############################################################################## 
param( 
        [string] $remoteHost = "localhost", 
        [int] $port = 80, 
        [switch] $UseSSL, 
        [string] $inputObject, 
        [int] $commandDelay = 100 
     )
[string] $output = ""

## Store the input into an array that we can scan over. If there was no input, 
## then we will be in interactive mode. 
$currentInput = $inputObject 
if(-not $currentInput) 
{ 
    $SCRIPT:currentInput = @($input) 
} 
$scriptedMode = [bool] $currentInput

function Main 
{ 
    ## Open the socket, and connect to the computer on the specified port 
    if(-not $scriptedMode) 
    { 
        write-host "Connecting to $remoteHost on port $port" 
    }

    trap { Write-Error "Could not connect to remote computer: $_"; exit } 
    $socket = new-object System.Net.Sockets.TcpClient($remoteHost, $port)

    if(-not $scriptedMode) 
    { 
        write-host "Connected. Press ^D followed by [ENTER] to exit.`n" 
    }

    $stream = $socket.GetStream() 
    
    if($UseSSL) 
    { 
        $sslStream = New-Object System.Net.Security.SslStream $stream,$false 
        $sslStream.AuthenticateAsClient($remoteHost) 
        $stream = $sslStream 
    }

    $writer = new-object System.IO.StreamWriter $stream

    while($true) 
    { 
        ## Receive the output that has buffered so far 
        $SCRIPT:output += GetOutput

        ## If we’re in scripted mode, send the commands, 
        ## receive the output, and exit. 
        if($scriptedMode) 
        { 
            foreach($line in $currentInput) 
            { 
                $writer.WriteLine($line) 
                $writer.Flush() 
                Start-Sleep -m $commandDelay 
                $SCRIPT:output += GetOutput 
            }

            break 
        } 
        ## If we’re in interactive mode, write the buffered 
        ## output, and respond to input. 
        else 
        { 
            if($output)  
            { 
                foreach($line in $output.Split("`n")) 
                { 
                    write-host $line 
                } 
                $SCRIPT:output = "" 
            }

            ## Read the user’s command, quitting if they hit ^D 
            $command = read-host 
            if($command -eq ([char] 4)) { break; }

            ## Otherwise, Write their command to the remote host 
            $writer.WriteLine($command) 
            $writer.Flush() 
        } 
    }

    ## Close the streams 
    $writer.Close() 
    $stream.Close()

    ## If we’re in scripted mode, return the output 
    if($scriptedMode) 
    { 
        $output 
    } 
}

## Read output from a remote host 
function GetOutput 
{ 
    ## Create a buffer to receive the response 
    $buffer = new-object System.Byte[] 1024 
    $encoding = new-object System.Text.AsciiEncoding

    $outputBuffer = "" 
    $foundMore = $false

    ## Read all the data available from the stream, writing it to the 
    ## output buffer when done. 
    do 
    { 
        ## Allow data to buffer for a bit 
        start-sleep -m 1000

        ## Read what data is available 
        $foundmore = $false 
        $stream.ReadTimeout = 3000

        do 
        { 
            try 
            { 
                $read = $stream.Read($buffer, 0, 1024)

                if($read -gt 0) 
                { 
                    $foundmore = $true 
                    $outputBuffer += ($encoding.GetString($buffer, 0, $read)) 
                } 
            } catch { $foundMore = $false; $read = 0 } 
        } while($read -gt 0) 
    } while($foundmore)

    $outputBuffer 
}

. Main