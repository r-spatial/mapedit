# get the MIME type using .NET's MimeMapping class

param (
  [string]$filePath
)

Add-Type -AssemblyName "System.Web"
$mimeType = [System.Web.MimeMapping]::GetMimeMapping($filePath)
Write-Host $mimeType
