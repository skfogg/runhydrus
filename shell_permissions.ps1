# Define the folder path
$folderPath = "C:\Program Files\PC-Progress\HYDRUS 5.06 64-bit\Bin"

# Validate that the folder exists
if (-Not (Test-Path -Path $folderPath -PathType Container)) {
    Write-Host "Error: Folder path does not exist." -ForegroundColor Red
    exit
}

try {
    # Get the current ACL (Access Control List)
    $acl = Get-Acl $folderPath

    # Define the access rule: Everyone, FullControl, Inheritance, Allow
    $accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule(
        "Everyone",
        "FullControl",
        "ContainerInherit,ObjectInherit",
        "None",
        "Allow"
    )

    # Add the new rule to the ACL
    $acl.SetAccessRule($accessRule)

    # Apply the updated ACL to the folder
    Set-Acl -Path $folderPath -AclObject $acl

    Write-Host "Full Control granted to 'Everyone' for $folderPath" -ForegroundColor Green
}
catch {
    Write-Host "Error setting permissions: $_" -ForegroundColor Red
}