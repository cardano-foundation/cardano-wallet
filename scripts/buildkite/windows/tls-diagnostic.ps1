Write-Host "=== Agent identity ==="
whoami
Write-Host "USERPROFILE: $env:USERPROFILE"

Write-Host "`n=== Cert store via .NET X509Store API ==="
try {
    $store = New-Object System.Security.Cryptography.X509Certificates.X509Store("Root", "CurrentUser")
    $store.Open("ReadOnly")
    Write-Host "CurrentUser ROOT: $($store.Certificates.Count) certs"
    $store.Close()
    $store2 = New-Object System.Security.Cryptography.X509Certificates.X509Store("Root", "LocalMachine")
    $store2.Open("ReadOnly")
    Write-Host "LocalMachine ROOT: $($store2.Certificates.Count) certs"
    $store2.Close()
} catch {
    Write-Host "FAILED: $_"
}

Write-Host "`n=== Test: curl.exe to github.com ==="
curl.exe -sI https://github.com 2>&1 | Select-Object -First 3

Write-Host "`n=== Test: Invoke-WebRequest to github.com ==="
try {
    $r = Invoke-WebRequest -Uri https://github.com -UseBasicParsing -TimeoutSec 10
    Write-Host "Status: $($r.StatusCode)"
} catch {
    Write-Host "FAILED: $_"
}

Write-Host "`n=== Test: .NET SslStream to github.com ==="
try {
    $tcp = New-Object System.Net.Sockets.TcpClient("github.com", 443)
    $ssl = New-Object System.Net.Security.SslStream($tcp.GetStream())
    $ssl.AuthenticateAsClient("github.com")
    Write-Host "TLS OK: Protocol=$($ssl.SslProtocol) Cipher=$($ssl.CipherAlgorithm)"
    Write-Host "Remote cert: $($ssl.RemoteCertificate.Subject)"
    Write-Host "Issuer: $($ssl.RemoteCertificate.Issuer)"
    $ssl.Close()
    $tcp.Close()
} catch {
    Write-Host "FAILED: $_"
}

Write-Host "`n=== Test: .NET SslStream to mithril aggregator ==="
try {
    $tcp = New-Object System.Net.Sockets.TcpClient("aggregator.release-preprod.api.mithril.network", 443)
    $ssl = New-Object System.Net.Security.SslStream($tcp.GetStream())
    $ssl.AuthenticateAsClient("aggregator.release-preprod.api.mithril.network")
    Write-Host "TLS OK: Protocol=$($ssl.SslProtocol) Cipher=$($ssl.CipherAlgorithm)"
    Write-Host "Remote cert: $($ssl.RemoteCertificate.Subject)"
    Write-Host "Issuer: $($ssl.RemoteCertificate.Issuer)"
    $ssl.Close()
    $tcp.Close()
} catch {
    Write-Host "FAILED: $_"
}

Write-Host "`n=== Test: CertOpenSystemStore via P/Invoke (what crypton-x509-system uses) ==="
try {
    Add-Type -TypeDefinition @"
    using System;
    using System.Runtime.InteropServices;
    public class CertStoreTest {
        [DllImport("crypt32.dll", SetLastError=true)]
        public static extern IntPtr CertOpenSystemStore(IntPtr hProv, string szSubsystemProtocol);
        [DllImport("crypt32.dll", SetLastError=true)]
        public static extern IntPtr CertEnumCertificatesInStore(IntPtr hCertStore, IntPtr pPrevCertContext);
        [DllImport("crypt32.dll", SetLastError=true)]
        public static extern bool CertCloseStore(IntPtr hCertStore, int dwFlags);
        public static int CountCerts(string storeName) {
            IntPtr store = CertOpenSystemStore(IntPtr.Zero, storeName);
            if (store == IntPtr.Zero) return -1;
            int count = 0;
            IntPtr cert = IntPtr.Zero;
            while ((cert = CertEnumCertificatesInStore(store, cert)) != IntPtr.Zero) {
                count++;
            }
            CertCloseStore(store, 0);
            return count;
        }
    }
"@
    Write-Host "CertOpenSystemStore('ROOT') count: $([CertStoreTest]::CountCerts('ROOT'))"
    Write-Host "CertOpenSystemStore('CA') count: $([CertStoreTest]::CountCerts('CA'))"
    Write-Host "CertOpenSystemStore('MY') count: $([CertStoreTest]::CountCerts('MY'))"
} catch {
    Write-Host "FAILED: $_"
}
