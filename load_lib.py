# Downloads proprietary bass2.4 audio library

import os
os.mkdir("lib_dl_temporary")
import platform
from urllib2 import urlopen, URLError, HTTPError
import zipfile

def dlfile(url):
    # Open the url
    try:
        f = urlopen(url)
        print "downloading " + url

        # Open our local file for writing
        with open(os.path.basename(url), "wb") as local_file:
            local_file.write(f.read())

    #handle errors
    except HTTPError, e:
        print "HTTP Error:", e.code, url
    except URLError, e:
        print "URL Error:", e.reason, url

def rm_rf(d):
    for path in (os.path.join(d,f) for f in os.listdir(d)):
        if os.path.isdir(path):
            rm_rf(path)
        else:
            os.unlink(path)
    os.rmdir(d)

# Check OS type
uname = platform.system()

if (uname == "Linux"):
    url = "http://uk.un4seen.com/files/bass24-linux.zip"
    lib_name = "libbass.so"

if (uname == "Darwin"):
    url = "http://uk.un4seen.com/files/bass24-osx.zip"
    lib_name = "libbass.dylib"

if (uname == "Windows"):
    url = "http://uk.un4seen.com/files/bass24.zip"
    lib_name = "bass.dll"

os.chdir("lib_dl_temporary")

# Download file
dlfile(url)

zip_name = os.path.basename(url)
# Extract zip
print "extracting " + zip_name
with zipfile.ZipFile(zip_name, "r") as z:
    z.extractall()

os.rename("./" + lib_name, "./../" + lib_name)

os.chdir("./..")
rm_rf("lib_dl_temporary")




















