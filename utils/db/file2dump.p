/** file2dump.p - Renames data files to the correct dumpfile name
                  for loading by load-data.w.
    Need to clear out bkup directory after check of before and after.
**/
                  
def var li-i as int no-undo.
def var lc-oname as char format "x(60)" no-undo.
def var lc-bname as char format "x(60)" no-undo.
def var lc-nname as char format "x(60)" no-undo.

def temp-table tt-opfile no-undo
    field oname as char format "x(30)"
    field fname as char format "x(30)"
    index main is primary unique
        oname.

def temp-table tt-dbfile no-undo
    field fname as char format "x(30)"
    field dname as char format "x(10)"
    index main is primary unique
        fname.
    
/** Input from a list of operating system filenames 
    left in /share/jonnyo-temp.
**/
input from /users/johnn/filename.txt.

repeat:
    create tt-opfile.
    import tt-opfile.
end.

input close.

/** Input a valid list of database filenames 
    and their dump names from all databases.
**/
input from /users/johnn/file2dump.txt.

repeat:
    create tt-dbfile.
    import tt-dbfile.
end.

for each tt-opfile:

    if tt-opfile.oname = "" then
    do:
        delete tt-opfile.
        next.
    end.
    
    li-i = index(tt-opfile.oname,".").
    tt-opfile.fname = substring(tt-opfile.oname,1,li-i - 1).
    
    /** Validate against DB File names **/
    find first tt-dbfile where
    tt-dbfile.fname = tt-opfile.fname
    no-error.
    
    /** Rename operating system file to the dumpname **/
    if available tt-dbfile then
    do:
        /** Move to a bkup directory, then
            copy back to the original directory
            with the correct file name. After checks,
            delete contents of bkup directory.
        **/
        lc-oname = "/share/jonnyo-temp/" + tt-opfile.oname.
        lc-bname = "/share/jonnyo-temp/bkup/" + tt-opfile.oname.
        lc-nname = "/share/jonnyo-temp/" + tt-dbfile.dname + 
                    substring(tt-opfile.oname,li-i).
        
        if search(lc-oname) ne ? then 
        do:
            /**
            display lc-oname at 1
                    lc-bname at 1
                    lc-nname at 1.
            **/
            
            os-command silent mv value(lc-oname) value(lc-bname).
            os-command silent cp -p value(lc-bname) value(lc-nname).
        end.
    end.
end.

