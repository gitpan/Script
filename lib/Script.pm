# Script - (Win32) system administrator`s library
#           - for login and application startup scripts, etc
#
# makarow and demed, 03-05/07/2000, 16-17/06/2000,
# 08/05/2000, 02/04/2000, 25/03/2000, 12-28/02/2000, 
# 16/12/99, 09/12/99, 05/12/99, 24/11/99, 08/11-19/10/99, 
# 02/07/99, 28/06/99, 23/06/99, 15/06/99, 01/04/99, 25/03/99, 23/03/99, 
# 20/03/99, 19/03/99, 17/03/99, 15/03/99, 13/03/99, 12/03/99, 09/03/99, 
# 06/03/99, 03/03/99, 02/03/99, 01/03/99, 27/02/99, 24/02/99, 18/02/99 13:04
#
package Script;
require 5.000;
require Exporter;
use     Carp;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
$VERSION = '0.50';
@ISA = qw(Exporter);
@EXPORT = qw(CPTranslate Die FileACL FileCompare FileCopy FileCRC FileDelete FileEdit FileFind FileGlob FileHandle FileIni FileMkDir FileNameMax FileNameMin FileRead FileSize FileSpace FileTrack FileWrite FTPCmd GUIMsg NetUse Pause Platform Registry Run SMTPSend StrTime UserEnvInit);
@EXPORT_OK = qw(Echo FileLog Print TrAnsi2Oem TrOem2Ansi Try(@) TryHdr);
%EXPORT_TAGS = ('ALL'=>[@EXPORT,@EXPORT_OK],'OVER'=>[]);

use vars qw($Interact $GUI $Echo $ErrorDie $Error $Print $Language);
$Interact   =1;   # interaction with user; no: $Script::Interact=0
$GUI        =1;   # use GUI interaction instead of terminal
$Echo       =1;   # set echo on
$ErrorDie   =0;   # die on errors: $Script::ErrorDie=1
$Error      ='';  # error result
$FileLog    ='';  # log file name (LOG handle) for Echo, Print, errors...
$Print      ='';  # external print routine hardlink
$Language   ='';  # language of user interaction, 'ru' may be

# FileHandle(\*STDOUT,sub{$| =1});
# FileHandle(\*STDERR,sub{$| =1});

1;

sub Try (@);

sub import {
 if (grep /^:OVER$/,@_) {
    my $lst =(grep /^:ALL$/, @_) ? $EXPORT_TAGS{ALL} : \@EXPORT;
    foreach my $elem (@$lst) {
      my $sym =caller(1) .'::' .$elem; undef(&$sym);
    }
 }
 $_[0]->export_to_level(1, @_);
}


############################################
sub CPTranslate {
 my ($f,$t,@s) =@_; 
 foreach my $v ($f, $t) {
   if    ($v =~/oem|866/i)   {$v ='ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôúõöùûü†°¢£§•Ò¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÏÎÍÌÓÔ'}
   elsif ($v =~/ansi|1251/i) {$v ='¿¡¬√ƒ≈®∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ‹€⁄›ﬁﬂ‡·‚„‰Â∏ÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘¸˚˙˝˛ˇ'}
   elsif ($v =~/koi/i)       {$v ='·‚˜Á‰Â≥ˆ˙ÈÍÎÏÌÓÔÚÛÙıÊË„˛˚˝¯˘ˇ¸‡Ò¡¬◊«ƒ≈£÷⁄… ÀÃÕŒœ–“”‘’∆»√ﬁ€›ÿŸﬂ‹¿—'}
   elsif ($v =~/8859-5/i)    {$v ='∞±≤≥¥µ°∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»…ÃÀ ÕŒœ–—“”‘’Ò÷◊ÿŸ⁄€‹›ﬁﬂ‡·‚„‰ÂÊÁËÈÏÎÍÌÓÔ'}
 }
 map {eval("~tr/$f/$t/")} @s; 
 @s >1 ? @s : $s[0];
}
sub TrOem2Ansi {CPTranslate('oem','ansi',@_)}
sub TrAnsi2Oem {CPTranslate('ansi','oem',@_)}

############################################
sub Die {
 my @txt = @_ ? @_ : $@;
 GUIMsg(($Language =~/ru/i ?'Œ¯Ë·Í‡' :'Error'), CPTranslate('oem','ansi',@txt)) 
       if $Interact && $GUI && !$^S;
 $! =1 if !$!;
 croak(join(' ',@txt))
}

############################################
sub Echo { !$Echo || Print(@_)}

############################################
sub FileACL {
Try eval { local $ErrorDie =1;
 my $opt =($_[0] =~/^\-/i ? shift : '');
 my $file=shift;
 my $sub =(ref($_[0]) eq 'CODE' ? shift : sub{1});
 my %acl =@_;
 my (%acd, %acf);
 Echo('FileACL',$opt,@_);
 eval('use Win32::FileSecurity');
 foreach my $k (keys(%acl)) {
   if    (ref($acl{$k}))          {$acd{$k} =Win32::FileSecurity::MakeMask(@$acl{$k}->[0]); $acf{$k} =Win32::FileSecurity::MakeMask(@$acl{$k}->[1])}
   elsif ($acl{$k} =~/full/i)     {$acd{$k} =Win32::FileSecurity::MakeMask(qw(FULL GENERIC_ALL)); $acf{$k} =Win32::FileSecurity::MakeMask(qw(FULL))}
   elsif ($acl{$k} =~/change/i)   {$acd{$k} =Win32::FileSecurity::MakeMask(qw(CHANGE GENERIC_WRITE GENERIC_READ GENERIC_EXECUTE)); $acf{$k} =Win32::FileSecurity::MakeMask(qw(CHANGE))}
   elsif ($acl{$k} =~/add&read/i) {$acd{$k} =Win32::FileSecurity::MakeMask(qw(ADD GENERIC_READ GENERIC_EXECUTE)); $acf{$k} =Win32::FileSecurity::MakeMask(qw(READ))}
   elsif ($acl{$k} =~/add&list/i) {$acd{$k} =Win32::FileSecurity::MakeMask(qw(ADD READ STANDARD_RIGHTS_READ STANDARD_RIGHTS_WRITE STANDARD_RIGHTS_EXECUTE READ_CONTROL SYNCHRONIZE))}
   # in doubt^
   elsif ($acl{$k} =~/add/i)      {$acd{$k} =Win32::FileSecurity::MakeMask(qw(STANDARD_RIGHTS_READ STANDARD_RIGHTS_WRITE STANDARD_RIGHTS_EXECUTE READ_CONTROL SYNCHRONIZE))}
   # in very doubt^
   elsif ($acl{$k} =~/read/i)     {$acd{$k} =Win32::FileSecurity::MakeMask(qw(READ GENERIC_READ GENERIC_EXECUTE)); $acf{$k} =Win32::FileSecurity::MakeMask(qw(READ))}
   elsif ($acl{$k} =~/list/i)     {$acd{$k} =Win32::FileSecurity::MakeMask(qw(READ_CONTROL SYNCHRONIZE STANDARD_RIGHTS_READ STANDARD_RIGHTS_WRITE STANDARD_RIGHTS_EXECUTE READ))}
   # in doubt^
 };
 FileFind($file
         ,sub{ Echo($_);
               if    (!&$sub(@_)) {}
               elsif ($_[0]->[2] & 0040000) {
                  if    (!scalar(%acd)) {my %h; Win32::FileSecurity::Get($_,\%h); foreach my $k (sort(keys(%h))){my @s; Win32::FileSecurity::EnumerateRights($h{$k},\@s); Echo($k,'=>',@s)}}
                  elsif ($opt =~/\+/i)  {my %h; Win32::FileSecurity::Get($_,\%h); foreach my $k (keys(%acd)){$h{$k}=$acd{$k}}; Win32::FileSecurity::Set($_,\%h)}
                  else  {Win32::FileSecurity::Set($_,\%acd)}
                  $_[0]->[2] =0 if $opt !~/r/i;
               }
               else {
                  if    (!scalar(%acf)) {my %h; Win32::FileSecurity::Get($_,\%h); foreach my $k (sort(keys(%h))){my @s; Win32::FileSecurity::EnumerateRights($h{$k},\@s); Echo($k,'=>',@s)}}
                  elsif ($opt =~/\+/i)  {my %h; Win32::FileSecurity::Get($_,\%h); foreach my $k (keys(%acf)){$h{$k}=$acf{$k}}; Win32::FileSecurity::Set($_,\%h)}
                  else  {Win32::FileSecurity::Set($_,\%acf)}
               }
             }
         )
},0}

############################################
sub FileCompare {
 my $opt =($_[0] =~/^\-/i ? shift : ''); 
 my $ret =eval("use File::Compare; compare(\@_)");
 if ($@ || $ret <0) {TryEnd(($Language =~/ru/i ?'ç•„§†Á≠Æ ·‡†¢≠•≠®•' :'Failure')." compare(" .join(', ',@_) ."): $!"); 0}
 else {$ret}
}

############################################
sub FileCopy {
Try eval { local $ErrorDie =1;
 my $opt =$_[0] =~/^-/i ? shift : '';
 my ($src,$dst) =@_;
 # 'd'irectory or 'f'ile hint; 'r'ecurse subdirectories, 'i'gnore errors
 $opt =~s/-//g;
 if ($ENV{OS} && $ENV{OS} =~/Windows_NT/i) {
    $src =~tr/\//\\/;
    $opt ="${opt}Z";
    $opt ="${opt}Y" if (eval('use Win32::TieRegistry; $$Registry{\'LMachine\\Software\\Microsoft\\Windows NT\\CurrentVersion\\\\CurrentVersion\'}') ||0) >=5;
 }
 elsif ($^O eq 'MSWin32') {
    $src =~tr/\//\\/;
    $dst =~tr/\//\\/;
 }
 if    ($^O ne 'MSWin32' && $^O ne 'dos') {
    # Echo('copy', @_);
    # eval ('use File::Copy; File::Copy::copy(\@_)') || croak($!);
    $opt =~ tr/fd//;
    $opt ="-${opt}dp";
    $opt =~ tr/ri/Rf/;
    Run('cp', $opt, @_);
 }
 elsif ($opt =~/[fd]/i && ($ENV{OS} && $ENV{OS}=~/windows_nt/i ? !-e $dst : !-d $dst)) {
    my $rsp =($opt =~/d/i ? 'D' : 'F');
    $opt =~s/(r)/SE/i; $opt =~s/(i)/C/i; $opt =~s/[fd]//ig; $opt =~s/(.{1})/\/$1/gi;
    my $cmd ="xcopy /H/R/K/Q$opt \"$src\" \"$dst\""; Echo($cmd);
    local *OUT; open(OUT, "|$cmd") || croak(($Language =~/ru/i ?'äÆØ®‡Æ¢†≠®•' :'Copying') ." $cmd : $?");
    print(OUT $rsp, "\n"); close(OUT);
    my $r =$?>>8;
    croak("$cmd : $r") if $r;
    !$r;
 }
 else {
    $opt =~s/(r)/SE/i; $opt =~s/(i)/C/i; $opt =~s/[fd]//ig; $opt =~s/(.{1})/\/$1/gi;
    Run("xcopy", "/H/R/K/Q$opt", "\"$src\"", "\"$dst\"");
 }
},0}

############################################
sub FileCRC {
Try eval { local $ErrorDie =1;
 my $opt =($_[0] =~/^\-/i ? shift : ''); 
 my ($file) =@_;
 my $bufsze =64*1024;
 my $buff;
 my $crc =0;
 local *IN;
 eval("use Compress::Zlib");
 open(IN, "<$file") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening') ." '<$file': $!");
 binmode(IN);
 while (!eof(IN)) {
   defined(read(IN, $buff, $bufsze)) || croak(($Language =~/ru/i ?'ó‚•≠®•' :'Reading')." '<$file': $!");
   $crc = $opt =~/\-a? ?adler/i ? adler32($buff,$crc) : crc32($buff,$crc);
 }
 close(IN) || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '<$file': $!");
 $crc;
},0}

############################################
sub FileDelete {
Try eval { local $ErrorDie =1;
 Echo('FileDelete',@_);
 my $opt =$_[0] =~/^\-/ || $_[0] eq '' ? shift : '';
 my $ret =1;
 foreach my $par (@_) {
   foreach my $elem (FileGlob($par)) {
     if (-d $elem) {                 # '-r' - recurse subdirectories
        if ($opt =~/r/i && !FileDelete($opt,"$elem/*")) {
              $ret =0
        }
        elsif (!rmdir($elem)) {
              $ret =0;
              croak(($Language =~/ru/i ?'ì§†´•≠®•' :'Deleting')." FileDelete('$elem'): $!");
        }
     }
     elsif (-f $elem && !unlink($elem)) {
           $ret =0;
           croak(($Language =~/ru/i ?'ì§†´•≠®•' :'Deleting')." FileDelete('$elem'): $!");
     }
   }
 }
 $ret
},0}

############################################
sub FileEdit {
Try eval { local $ErrorDie =1;
 Echo("FileEdit",@_);
 my $opt    = $_[0] =~/^-/i ? shift : '-i';
 my $file   = shift;
 my $fileto = @_ >1 ? shift : ''; if($fileto =~/^-/i) {$opt =$opt .$fileto; $fileto =''};
 my $sub    = shift;
 my $mtd    = $opt =~/^\-i/i ? 1 : 0;
 my ($sct,@v) =('','','','');
 local $_;

 if    ($opt =~/^\-i$/i) {           # '-i' - default, in memory inplace edit
       my @dta;
       $mtd =0;
       foreach my $row (FileRead($file)) {
          $_ =$row;
          $sct =$1 if /^ *[\[]([^\]]*)/;
          &{$sub}(@v); # &{$sub}($sct, @v);
          $mtd =1 if !defined($_) || $_ ne $row;
          push(@dta, $_) if defined($_);
       }
       return(!$mtd || FileWrite($file, @dta));
 }
 elsif ($opt =~/^-m$/i) {            # '-m' - multiline edit in memory
       $fileto = $_ =FileRead($file);
       &{$sub}(@v); # &{$sub}($sct, @v);
       return(($fileto eq $_) || FileWrite($file, $_));
 }
                                     # '-i ext' or 'from, to'
 $fileto ="$file.$1" if $opt =~/^\-i *(.*)/i;
 if (!-f $file && -f $fileto) {
    Echo("copy", $fileto, $file);
    eval ("use File::Copy");
    File::Copy::copy ($fileto, $file) || croak(($Language =~/ru/i ?'äÆØ®‡Æ¢†≠®•' :'Copying')." '$fileto'->'$file': $!");
 }
 local (*IN, *OUT);
 open(IN, "<$file")    || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '<$file': $!");
 open(OUT, ">$fileto") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '>$fileto': $!");
 while (!eof(IN)) {
   defined($_ =<IN>) || croak("ó‚•≠®• '<$file': $!");
   chomp;
   $sct =$1 if /^ *[\[]([^\]]*)/;
   &{$sub}(@v); # &{$sub}($sct, @v);
   !defined($_) || print(OUT $_,"\n") || croak(($Language =~/ru/i ?'á†Ø®·Ï' :'Writing')." '>$fileto': $!");
 }
 close(IN)  || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '<$file': $!");
 close(OUT) || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '>$fileto': $!");
 !$mtd || rename($fileto, $file) || croak(($Language =~/ru/i ?'è•‡•®¨•≠Æ¢†≠®•' :'Renaming')." '$file'->'$fileto': $!");
 1;
},0}

############################################
sub FileFind {
Try eval { local $ErrorDie =1;
 my $opt =($_[0] =~/^\-/i ? shift : '');
 my ($sub, $i, $ret) =(0,0,0);
 local ($_, $result) if $opt !~/-\$/i;
 $opt =$opt ."-\$"   if $opt !~/-\$/i;
 foreach my $dir (@_) {
   $i++;
   if    ((!$sub || ref($dir)) && ref($_[$#_]) && $i <=$#_) {
         foreach my $elem (@_[$i..$#_]){if(ref($elem)){$sub =$elem; last}};
         next if ref($dir)
   }
   elsif (ref($dir)) {
         $sub =$dir; next
   }
   foreach my $elem (FileGlob($dir)) {
     $_ =$elem;
     my @stat =stat($elem);
     my @nme  =(/^(.*)[\/\\]([^\/\\]+)$/ ? ($1,$2) : ('',''));
     if    (@stat ==0 && ($opt =~/[^!]*i/i || $elem =~/[\?]/i)) {next} # bug in stat!
     elsif (@stat ==0) {croak(($Language =~/ru/i ?'ç•„§†Á•≠' :'Failure')." stat('$elem'): $!"); undef($_)}
     elsif ($stat[2] & 0040000 && $opt =~/![^d]*d/i) {}
     elsif (&$sub(\@stat,@nme,$result)) {$ret +=1}; # $_[3] - optional result
     defined($_) || return(0);                      # error stop: undef($_)
     if ($stat[2] & 0040000 && $opt !~/![^r]*r/i) { # no recurse: $_[0]->[2] =0
        $ret +=FileFind($opt, "$elem/*", $sub);
        defined($_) || return(0);
     }
   }
 }
 defined($result) ? $result : $ret
},0}

############################################
sub FileGlob {
 $^O eq 'MSWin32'
 ? eval("use File::DosGlob 'glob'; File::DosGlob::glob(\@_)")
 : glob(@_)
}

############################################
sub FileHandle {
Try eval { local $ErrorDie =1;
 my ($file,$sub)=@_;
 my $hdl =select();
 my $ret;
 if (ref($file) || ref(\$file) eq 'GLOB') {select(*$file); $ret =&$sub($hdl); select($hdl)}
 else {
   local *HANDLE; open(HANDLE, $file) || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '$file': $!");
   select(HANDLE); $ret =&$sub($hdl); select($hdl);
   close(HANDLE) || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '$file': $!");
 }
 $ret;
},''}

############################################
sub FileIni {
Try eval { local $ErrorDie =1;
 my $opt    =$_[0] =~/^-/i ? shift : '';
 my $file   =shift;
 Echo("FileIni",$opt,$file);
 my @ini    =FileRead($file);
 my ($sct, $nme, $val, $op);
 my ($isct, $inme, $iins, $val1) =(-1);
 my $mod    =0;

 # Return hash with ini-file data:
 if (scalar(@_)<=0) {     
    my %dta;
    foreach my $row (@ini) {
      $row =~/^ *(.*?) *$/; $row =$1;
      if    ($row =~/^[\[]/i) {$sct =$row; $dta{$sct}={}}
      elsif ($row =~/^[;]/i)  {}
      else  {$row =~/^([^\=]*?) *= *(.*)/i; $dta{$sct}->{$1}=$2;}
    }
    return(\%dta);
 }

 # Edit ini-file with @_ entries:
 #      '[section]'    ,  ';comment'    , [data,value]    or
 #     ['[section]',op], [';comment',op], [data,value,op]
 # op: '+'set (default), '-'del, ';'comment, 'i'nitial vaue, 'o'ptional value
 foreach my $row (@_) {   
   if    ((ref($row) ? $$row[0] : $row) =~/^ *[\[]/i) {
         $sct =ref($row) ? $$row[0] : $row; $nme =undef; $val =undef;
         $op  =ref($row) ? $$row[1] || '+' : '+';
         $isct=-1;
         for(my $i =0; $i <=$#ini; $i++) {
           next if !$ini[$i];
           if (index(($ini[$i]=~/^ *(.*?) *$/,uc($1)),uc($sct))>=0) {$isct =$i; last};
         }
         # print "$sct : $isct : ".($isct==-1 ? "" : $ini[$isct])."\n";
         if    ($op =~/[\+i]/i && $isct ==-1) {$mod =1; push(@ini, $sct); $isct =$#ini}
         elsif ($isct ==-1)                   {}
         elsif ($op =~/[\;]/i) {
               $mod =1; $ini[$isct] =';' .$ini[$isct];
               for(my $i =$isct+1; $i <=$#ini && $ini[$i] !~/^ *[\[]/i; $i++) {
                 $ini[$i] =';' .$ini[$i]
               }
         }
         elsif ($op =~/[\-]/i) {
               $mod =1; undef($ini[$isct]);
               for(my $i =$isct+1; $i <=$#ini && $ini[$i] !~/^ *[\[]/i; $i++) {
                 undef($ini[$i])
               }
         }
   }
   elsif ((ref($row) ? $$row[0] : $row) =~/^ *[\;]/i) {
         $nme =ref($row) ? $$row[0] : $row; $val =undef;
         $op  =ref($row) ? $$row[1] || '+' : '+';
         $inme=-1; $iins =$#ini +1;
         for(my $i =$isct+1; $i <=$#ini; $i++) {
           next if !$ini[$i];
           if ($ini[$i] =~/^ *[\[]/i) {$iins =$i; last}
           if (index(($ini[$i]=~/^ *(.*?) *$/,uc($1)),uc($nme))>=0) {$inme =$i; last}
         }
         if    ($op =~/[\-]/i && $inme !=-1) {$mod =1; undef($ini[$inme])}
         elsif ($op =~/[\+]/i && $inme ==-1) {$mod =1; splice(@ini, $iins, 0, $nme)}
   }
   else {
         $nme =$$row[0]; $val =$$row[1];
         $op  =$$row[2] || (!defined($$row[1]) ? '-' : '+');
         $inme=-1; $iins =$#ini +1; $val1='';
         for(my $i =$isct+1; $i <=$#ini; $i++) {
           next if !$ini[$i];
           if ($ini[$i] =~/^ *[\[]/i) {$iins =$i; last}
           if (index(($ini[$i]=~/^ *(.*?) *$/,uc($1)),uc($nme))>=0) 
              {$inme =$i; $val1 =$1 if $ini[$i]=~/= *(.*?) *$/i; last}
         }
         # print "$nme=>$val : [$inme..$iins] : $val1\n";
         if    ($op =~/[\+i]/i  && $inme ==-1)  {$mod =1; splice(@ini, $iins, 0, "$nme=$val")}
         elsif ($inme ==-1)                     {}
         elsif ($op =~/[;]/i)                   {$mod =1; $ini[$inme] =';'.$ini[$inme]}
         elsif ($op =~/[\-]/i)                  {$mod =1; undef($ini[$inme])}
         elsif ($op =~/[\+o]/ && $val ne $val1) {$mod =1; $ini[$inme] ="$nme=$val"}
   }
 }
 !$mod || FileWrite($file,@ini);
},0}

############################################
sub FileLog {
Try eval {
 return $FileLog if !@_;
 return (close(LOG),$FileLog ='') if @_ && !defined($_[0]) && $FileLog ne '';
 open(LOG, ">>$_[0]") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '>>$_[0]': $!");
 $SIG{__WARN__} =sub{Print(@_)};
 $SIG{__DIE__}  =sub{$^S ? die(@_) : Print(@_)};
 $FileLog =$_[0];
},''}

############################################
sub FileMkDir {
Try eval { local $ErrorDie =1;
 my ($dir, $mask) =@_;
 Echo('mkdir', @_);
 mkdir($dir, $mask || 0777) || croak(($Language =~/ru/i ?'ëÆß§†≠®•' :'Creating').' '.join(', ',@_) .": $!");
},0}

############################################
sub FileNameMax {
 my ($dir, $sub) =@_;
 my ($max, $nme) =(undef,'');
 local $_;
 eval { local $ErrorDie =1;
  foreach my $elem (FileGlob($dir =~/[\?\*]/ ? $dir : "$dir/*")) {
    next if !$elem || -d $elem;
    my $nmb =($sub ? &$sub($elem, ($_ =$elem =~/([^\\\/]+)$/i ? $1 :''), ($elem =~/([\d]+)[^\\\/]*$/ ? $1 : undef))
                   : ($elem =~/([\d]+)[^\\\/]*$/ ? $1 : undef));
    if (defined($nmb) && (!$max || $max <$nmb)) {$max =$nmb; $nme =$elem};
  }
 }; if ($@) {$max =undef; $nme =''; TryEnd()}
 wantarray ? ($nme, $max) : $max;
}

############################################
sub FileNameMin {
 my ($dir, $sub) =@_;
 my ($min, $nme) =(undef,'');
 local $_;
 eval { local $ErrorDie =1;
  foreach my $elem (FileGlob($dir =~/[\?\*]/ ? $dir : "$dir/*")) {
    next if !$elem || -d $elem || $elem !~/([\d]+)[^\\\/]*$/;
    my $nmb =($sub ? &$sub($elem, ($_ =$elem =~/([^\\\/]+)$/i ? $1 :''), ($elem =~/([\d]+)[^\\\/]*$/ ? $1 : undef))
                   : ($elem =~/([\d]+)[^\\\/]*$/ ? $1 : undef));
    if (defined($nmb) && (!$min || $min >$nmb)) {$min =$nmb; $nme =$elem;}
  }
 }; if ($@) {$min =undef; $nme =''; TryEnd()}
 wantarray ? ($nme, $min) : $nme;
}

############################################
sub FileRead {
 my $opt =($_[0] =~/^\-/i ? shift : ''); # 'a'rray, 's'calar, 'b'inary
    $opt =$opt .'a' if $opt !~/[asb]/i && wantarray;
 my ($file, $sub) =@_;
 my ($row, @rez);
 local *IN;
 eval { local $ErrorDie =1;
  open(IN, "<$file") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '<$file': $!");
  if    ($sub) {
        $row  =1;
        local $_;
        while (!eof(IN)) {
          defined($_ =<IN>) || croak(($Language =~/ru/i ?'ó‚•≠®•' :'Reading')." '<$file': $!");
          chomp;
          $opt=~/a/i ? &$sub() && push(@rez,$_)
                     : &$sub();
        }
  }
  elsif ($opt=~/a/i) {
        while (!eof(IN)) {
          defined($row =<IN>) || croak(($Language =~/ru/i ?'ó‚•≠®•' :'Reading')." '<$file': $!");
          chomp($row);
          push (@rez, $row);
        }
  }
  else {
        binmode(IN) if $opt =~/b/i;
        defined(read(IN, $row, -s $file)) || croak(($Language =~/ru/i ?'ó‚•≠®•' :'Reading')." '<$file': $!");
  }
  close(IN) || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '<$file': $!");
 }; if ($@) {@rez =(); $row =''; TryEnd()}
 $opt=~/a/i ? @rez : $row
}

############################################
sub FileSize {
 my $opt =($_[0] =~/^\-/i ? shift : '-i');
 my $file=shift;
 my $sub =(ref($_[0]) ? shift : sub{1});
 FileFind($opt,$file, sub{$_[3] +=$_[0]->[7] if &$sub(@_)})
}

############################################
sub FileSpace {
Try eval { local $ErrorDie =1;
 my $disk =$_[0] || "c:\\";
 my $sze;
 if ($^O eq 'MSWin32') { $sze =`\%COMSPEC\% /c dir $disk`=~/([\d\.\xFF ]+)[\D]*$/i ? $1 : '' }
 else                  { $sze  =`df -k` =~/^$disk +([\d]+)/im ? $1 : ''}
 $sze =~ s/[\xFF ]//g;
 $sze eq '' && croak("FileSpace($disk) -> $?)");
 $sze
},0}

############################################
sub FileTrack {
Try eval { local $ErrorDie =1;
 my $opt =($_[0] =~/^\-/i ? shift : '-'); 
 my ($src,$dst,$sub) =@_;
 my $lvl =1;
 my $chg ='';
 local ($_, %dbm, *LOG) if $opt !~/-\$/i;
 if ($opt !~/-\$/i) {
    Echo('FileTrack',$opt,@_);
    $opt =$opt ."-\$";
    dbmopen(%dbm, "$dst/FileTrack", 0666)
    && open(LOG,">>$dst/FileTrack.log") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '$dst/FileTrack': $!");
    $dst =$dst ."/" .StrTime('yyyy-mm-dd_hh_mm_ss');
    $sub =sub{1} if !$sub;
    $lvl =0;
 }
 foreach (FileGlob("$src/*")) {
     my @stat =stat;
     my @nme  =(/^(.*)[\/\\]([^\/\\]+)$/ ? ($1,$2) : ('',''));
     if    (@stat ==0 && (/[^!]*i/i || /[\?]/i))  {next} # bug in stat!
     elsif (@stat ==0) {croak(($Language =~/ru/i ?'ç•„§†Á•≠' :'Failure')." stat('$_'): $!"); undef($_)}
     elsif ($stat[2] & 0040000 && $opt =~/![^d]*d/i) {}
     elsif (!&$sub(\@stat,@nme))  {next}
     elsif (!defined($_))         {return('')}  # err stop: undef($_)
     my $crc =$stat[2] & 0040000 || $opt !~/[^!]*t/i ? 0 : FileCRC($_);
     my $tst =!$dbm{$_} ? 'I'
             :$dbm{$_} !~/^([\d]+)\t([\d]+)$/ ? '?'
             :$1 != $stat[9] ? 'U'
             :$2 != $crc     ? 'C'
             :undef;
     if ($tst) {
        if    (($opt =~/![^c]c/i) || ($stat[2] & 0040000)) {} # bug in win95 xcopy!
        elsif (eval {FileCopy('-d',$_,$dst)}) {}
        elsif ($opt =~/[^!]*i/i) {next}
        else  {croak(@_)}
        $chg =1;
        print LOG StrTime(), "\t$tst\t$_\t",StrTime($stat[9]),"\t$crc\t$dst/$nme[1]\n";
        $dbm{$_} =$stat[9] ."\t" .$crc;
     }
     if ($stat[2] & 0040000 && $opt !~/![^r]*r/i) { # no recurse: $_[0]->[2] =0
        $chg =FileTrack($opt, "$src/$nme[1]", "$dst/$nme[1]", $sub) || $chg;
        defined($_) || return(0);
     }
 }
 if (!$lvl) {
    foreach (keys(%dbm)) {
      next if -e $_;
      my ($tme,$crc) =$dbm{$_} !~/^([\d]+)\t([\d]+)$/ ? (0,0) : ($1,$2);
      print LOG StrTime(), "\tD\t$_\t",StrTime($tme),"\t$crc\n";
      delete($dbm{$_});
    }
    dbmclose(%dbm)
    && close(LOG) || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '$dst/FileTrack': $!");
    return(-d $dst ? $dst : '') if $chg;
 }
 $chg
}, ''}

############################################
sub FileWrite {
Try eval { local $ErrorDie =1;
 my $opt  =($_[0] =~/^\-/i ? shift : ''); # 'b'inary
 my $file =shift;
 Echo("FileWrite",$file);
 local *OUT;
 open(OUT, ">$file") || croak(($Language =~/ru/i ?'é‚™‡Î‚®•' :'Opening')." '>$file': $!");
 if ($opt=~/b/i) {
     binmode(OUT);
     print(OUT @_)   || croak(($Language =~/ru/i ?'á†Ø®·Ï' :'Writing')." '>$file': $!");
 }
 else {
   foreach my $row (@_) {
     !defined($row)  || print(OUT $row, "\n") || croak(($Language =~/ru/i ?'á†Ø®·Ï' :'Writing')." '>$file': $!");
   }
 }
 close(OUT)          || croak(($Language =~/ru/i ?'á†™‡Î‚®•' :'Closing')." '>$file': $!");
},0}

############################################
sub FTPCmd {
 my ($host,$usr,$passwd,$cmd) =(shift,shift,shift,shift);
 Echo('FTPCmd',$host,$usr,$cmd,@_);
 eval { local $ErrorDie =1;
  my $ftp =eval("use Net::FTP; Net::FTP->new(\$host);") || croak("FTP $host: $@");
  $ftp->login($usr, $passwd) || ($ftp->close, croak("FTP '${usr}\@${host}': $@"));
  if ($cmd =~/^ascii|bin|ebcdic|byte/) {
     $cmd =~s/^bin$/binary/;
     eval("\$ftp->$cmd") || ($ftp->close, croak("FTP ${usr}\@${host} $cmd: $@"));
     $cmd =shift;
  }
  my @ret = ref($cmd) eq 'CODE' ? &$cmd($ftp) : eval("\$ftp->$cmd(\@_)");
  $ftp->close;
  ($cmd =~/dir|ls/ ? $@ : !$ret[0]) && croak("FTP ${usr}\@${host} $cmd(".join(', ',@_)."): $@");
 }; if ($@) {@ret =(); TryEnd()}
 $cmd =~/dir|ls/ ? @ret : $ret[0];
}

############################################
sub GUIMsg {
Try eval { local $ErrorDie =1;
 my $title = @_ >1 ? shift : '';
 return(0) if !$Interact;
 if (!$GUI) {map {Echo($_)} CPTranslate('ansi','oem',@_); return(Pause())};
 eval("use strict; use Tk");
 my $main  = new MainWindow (-title => $title);
 $main->Label (-text => "\n" .join("\n", @_) ."\n"
              ,-font => "System"
              ) -> pack(-fill => 'x');
 $main->Button(-text => ($Language =~/ru/i ?'«‡Í˚Ú¸' :'Close')
              ,-font => 'System'
              ,-command => sub{$main->destroy}
              )->pack->focus();
 $main->bind('Tk::Button','<Key-Return>'
            ,sub{my $r =$main->focusCurrent->cget('-command'); 
                 $r =~/array/i ? &{$$r[0]} : &$r });
 $main->bind('<Key-Escape>',sub{$main->destroy});
 $main->bind('<FocusOut>',sub{$main->focusForce});
 $main->grabGlobal;
 $main->focusForce;
 $main->update();
 $main->geometry('+'.int(($main->screenwidth() -$main->width())/2.2) 
                .'+'.int(($main->screenheight() -$main->height())/2.2));
 eval("MainLoop()");
},0}

############################################
sub NetUse {
 my ($d)=@_; 
 eval {`net use $d /delete`};
 Run('net','use',@_);
}

############################################
sub Pause {
Try eval { local $ErrorDie =1;
 if (@_) {print(join(' ',@_))}
 else    {print(($Language =~/ru/i ?'ç†¶¨®‚•' :'Press')." 'Enter'...")}
 return('') if !$Interact;
 my $r =<STDIN>;
 chomp($r);
},''}

############################################
sub Platform {
Try eval { local $ErrorDie =1;
 if    ($_[0] =~/^os$/i) {
   $ENV{OS}
   ? $ENV{OS} 
   : $^O eq 'MSWin32'
     ? eval('use Win32::TieRegistry; my $v =$$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\Version\'}; $v =~s/ /_/ig; $v') || 'Windows_95'
     : $^O  # 'Dos'
 }
 elsif ($_[0] =~/^osname$/i) {
   ($^O eq 'MSWin32'
    ? eval('use Win32::TieRegistry;$$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\Version\'}') ||''
    : '')
   || (`\%COMSPEC\% /c ver` =~/\n*([^\n]+)\n*/i ? $1 : '')
   || $ENV{OS} || $^O
 }
 elsif ($_[0] =~/^win32$/i) {
   $^O eq 'MSWin32' ? $ENV{windir} : ''
 }
 elsif ($_[0] =~/^ver/i) {
   my $v =
   ($^O eq 'MSWin32'
    ? eval('use Win32::TieRegistry; my $v =
          ($$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\VersionNumber\'} || $$Registry{\'LMachine\\Software\\Microsoft\\Windows NT\\CurrentVersion\\\\CurrentVersion\'} || \'\')
          .".".
          ($$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\SubVersionNumber\'} || $$Registry{\'LMachine\\Software\\Microsoft\\Windows NT\\CurrentVersion\\\\CurrentBuildNumber\'} || \'\')
          ; $v =~s/ //ig; $v')
   : '')
   || (`\%COMSPEC\% /c ver` =~/(Version|•‡·®Ô) *([^ \]]+)/im ? $2 : '');
   (@_ >1 ? [split(/\./,$v)]->[$_[1]] ||'' : $v);
 }
 elsif ($_[0] =~/^(patch)/i) {
    $^O eq 'MSWin32'
    ? eval('use Win32::TieRegistry; $$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\CSDVersion\'} || $$Registry{\'LMachine\\Software\\Microsoft\\Windows NT\\CurrentVersion\\\\CSDVersion\'}') || ''
    : ''
 }
 elsif ($_[0] =~/^lang$/i) {
   `\%COMSPEC\% /c dir c:\\` =~/·¢Æ°Æ§≠Æ$/i ? 'ru' : '';
 }
 elsif ($_[0] =~/^prodid$/i) {
    $^O eq 'MSWin32'
    ? eval('use Win32::TieRegistry;$$Registry{\'LMachine\\Software\\Microsoft\\Windows\\CurrentVersion\\\\ProductId\'} || $$Registry{\'LMachine\\Software\\Microsoft\\Windows NT\\CurrentVersion\\\\ProductId\'}') || ''
    : ''
 }
 elsif ($_[0] =~/^name$/i) {
   $ENV{COMPUTERNAME}
   ? lc($ENV{COMPUTERNAME})
   : $^O eq 'MSWin32'
     ? lc(eval('use Win32::TieRegistry; $$Registry{\'LMachine\\\\System\\\\CurrentControlSet\\\\Control\\\\ComputerName\\\\ComputerName\\\\\\\\ComputerName\'}'))
     : `net config` =~/(Computer name|äÆ¨ØÏÓ‚•‡) *\\*([^ ]+)$/im 
       ? lc($2)
       : Platform('host');
 }
 elsif ($_[0] =~/^hostdomain$/i) { #[gethostbyname('')]->[0] =~/[^\.]*\.(.*)/ ? $1 : '';
   eval('use Net::Domain;Net::Domain::hostdomain')
 }
 elsif ($_[0] =~/^host$/i) { #[gethostbyname('')]->[0]
   eval('use Sys::Hostname;hostname')
 }
 elsif ($_[0] =~/^domain|userdomain$/i) {
   $ENV{USERDOMAIN} || ''
 }
 elsif ($_[0] =~/^user$/i) {
  # $ENV{USERNAME} || $ENV{LOGNAME}
  # ? $ENV{USERNAME} || $ENV{LOGNAME}
  # : `net config` =~/(User name|èÆ´ÏßÆ¢†‚•´Ï) *([^ ]+)$/im 
  #   ? lc($2)
  #   : $ENV{windir}
  #     ? lc(eval("use Win32::TieRegistry; \$\$Registry{'LMachine\\\\System\\\\CurrentControlSet\\\\Control\\\\\\\\Current User'}"))
  #     : `logname` || ''
  getlogin()
 }
 else {''}
},''}

############################################
sub Print { 
 if ($Print) {&$Print(@_)}
 else { print(join(' ',@_), "\n");
        print LOG join(' ',StrTime(),@_), "\n" if $FileLog;
 }
}

############################################
sub Registry {
Try eval { local $ErrorDie =1;
 my $opt =($_[0] =~/^\-/i ? shift : '');
 my $dlm =$opt =~/\-([\|\/\\])/ ? $1 : '\\';
 my $key =shift;
 eval("use Win32::TieRegistry; \$Registry->Delimiter(\$dlm)");
 return (eval("\$\$Registry{\$key}")) if @_ ==0;
 my ($type)=@_ >1 ? shift : '';
 return(eval("delete(\$\$Registry{\$key})")) if @_ >0 && !defined($_[0]);
 my ($val) =@_;
 if ($type && $type !~/^REG_/i && $val =~/^REG_/i) {$val =$type; $type =$_[0]};
 my ($k, $h, $n);
 $k   =rindex($key,"$dlm$dlm");
 if ($k<0) {$k =rindex($key,$dlm); $n =substr($key, $k +1)}
 else      {$n =substr($key, $k +2)}
 $key =substr($key, 0, $k);
 $k   =$key;
 while(!eval("\$\$Registry{\$k}")) {
   $h ={substr($k, rindex($k,$dlm)+1)=>($h ? $h : {})};
   $k = substr($k, 0, rindex($k,$dlm));
 }
 eval("\$\$Registry{\$k} =\$h") if $h;
 if ($type) {eval("\$\$Registry{\$key}->SetValue(\$n,\$val,\$type)")}
 else       {eval("\$\$Registry{\$key.'\\\\'.\$n} =\$val")}
},''}

############################################
sub Run {
Try eval { local $ErrorDie =1;
 Echo(@_); system(@_); 
 my $r =$?>>8; #($?>>8 || $!);
 croak(join(' ',@_).": $r") if $r;
 !$r
},0}

############################################
sub SMTPSend {
Try eval { local $ErrorDie =1;
 my $host =shift;
 my $from =$_[0] !~/:/ ? shift : undef;
 my $to   =ref($_[0])  ? shift : undef;
 foreach my $r (@_) {last if $from && $to;
   if    (ref($r))  {$to =$r; $r ='To:'.join(',',@$r)}
   elsif (!$from && $r=~/^(from|sender):(.*)/i) {$from =$2}
   elsif (!$to   && $r=~/^to:(.*)/i)            {$to   =[split /,/,$1]}
 }
 Echo('SMTPSend',"$host, $from -> ".join(',',@$to));
 my $smtp =eval("use Net::SMTP; Net::SMTP->new(\$host)"); 
 $@     && croak($@);
 !$smtp && croak("SMTP Host $host");
 $smtp->mail($from) ||croak("SMTP From: $from");
 $smtp->to(@$to) ||croak("SMTP To: ".join(', ',@$to));
 $smtp->data(join("\n",@_)) ||croak("SMTP Data");
 $smtp->dataend() ||croak("SMTP DataEnd");
 $smtp->quit;
 1
},0}

############################################
sub StrTime { 
 my $msk =@_ ==0 || $_[0] =~/^\d+$/i ? ($Language =~/ru/i ? 'dd.mm.yy hh:mm:ss' : 'yyyy-mm-dd hh:mm:ss') : shift;
    $msk ='yyyymmddhhmmss' if !$msk;
 my @tme =@_ ==0 ? localtime(time) : @_ ==1 ? localtime($_[0]) : @_;
 $msk =~s/yyyy/sprintf('%04u',$tme[5] +1900)/ie;
 $tme[5] >=100 ? $msk =~s/yy/sprintf('%04u',$tme[5] +1900)/ie
               : $msk =~s/yy/sprintf('%02u',$tme[5])/ie;
 $msk =~s/mm/sprintf('%02u',$tme[4]+1)/ie;
 $msk =~s/dd/sprintf('%02u',$tme[3])/ie;
 $msk =~s/hh/sprintf('%02u',$tme[2])/ie;
 $msk =~s/mm/sprintf('%02u',$tme[1])/ie;
 $msk =~s/ss/sprintf('%02u',$tme[0])/ie;
 $msk
}

############################################
sub Try (@) {
 my $ret;
 local ($TrySubject, $TryStage) =('','');
 { local $ErrorDie =1;
   $ret = @_ >1 && ref($_[0]) eq 'CODE' ? eval {&{$_[0]}} : $_[0];
 }
 if   (!$@) {$ret} 
 else {
   my $err =$@ =$Error =$TrySubject .($TryStage eq '' ? '' : ": $TryStage:\n") .$@;
   $ret =ref($_[$#_]) eq 'CODE' ? &{$_[$#_]}() : $_[$#_]; 
   $@ ="$err\n$@" unless $@ eq $err;
   if ($ErrorDie) {$^S ? die($err) : Die($err)}
   elsif ($Echo && ref($_[$#_]) ne 'CODE') {warn("Error: $@")}
   $ret
 }
}

############################################
sub TryEnd {
 return(0) if !$@ && !@_;
 my $ert =@_;
 my $err =$Error =(@_ ? join(' ',@_) : $@);
 if ($ErrorDie) {$^S ? ($ert ? croak($err) : die($err)) : Die($err)}
 elsif  ($Echo) {$err ="Error: $err"; ($ert ? carp($err) : warn($err))}
 0
}

############################################
sub TryHdr {
 $TrySubject =$_[0] if defined($_[0]);
 $TryStage   =$_[1] if defined($_[1]);
 $Echo && Print($TrySubject.($TryStage ne '' ? ": $TryStage" : $TryStage)."...");
 ''
}

############################################
sub UserEnvInit {
Try eval { local $ErrorDie =1;
 return(0) if $^O ne 'MSWin32';
 my $opt =$_[0]    || 'nh'; $opt ='nhy' if $opt =~/^y$/i;
 my $os  =Platform('os');

 if ($opt =~/n/i && ($os !~/Windows_NT/i)){
    (!$ENV{OS} || $opt =~/y/i)  && ($ENV{OS} =$os)
                                && Run("winset","OS=$ENV{OS}");
    (!$ENV{COMPUTERNAME} || $opt =~/y/i) && ($ENV{COMPUTERNAME} =Platform('name'))
                                         && Run("winset","COMPUTERNAME=$ENV{COMPUTERNAME}");
    (!$ENV{USERNAME} || $opt =~/y/i) && ($ENV{USERNAME} =Platform('user'))
                                     && Run("winset","USERNAME=$ENV{USERNAME}"); # may be wrong after relogon!
 }

 my $home    = "c:\\Home";
 return(1) if $opt !~/h/i || !-d $home || !$ENV{USERNAME} || (($ENV{HOME}||'?')=~/\\$ENV{USERNAME}$/i);
 my $homeusr = $home ."\\" .ucfirst(lc($ENV{USERNAME}));
 my $homewrk=(-d "$home\\Work" ? "$home\\Work" : $home);
 if (!-d $homeusr) {
    FileMkDir($homeusr, 0700) ||return(0);
    if ($os =~/Windows_NT/i) {
       Run('cacls.exe',$homeusr,'/E','/C','/G',"$ENV{USERDOMAIN}\\$ENV{USERNAME}:F");
       eval("use Win32::FileSecurity");
       my %acl; Win32::FileSecurity::Get($homeusr,\%acl);
       foreach my $k (keys(%acl)) {
         if ($k !~/\\($ENV{USERNAME}|System|—»—“≈Ã¿|Administrator|¿‰ÏËÌËÒÚ‡ÚÓ)/i) 
            {Run('cacls.exe',$homeusr,'/E','/C','/R','"'.($k =~/ [^\\]*\\(.*)/ ? $1 : $k).'"')}
       }
    }
 }
 eval ("use Win32::TieRegistry;
        \$\$Registry{'CUser\\\\Software\\\\Microsoft\\\\Windows\\\\CurrentVersion\\\\Explorer\\\\User Shell Folders\\\\\\\\Personal'}
        = (lc(\$os) ne 'windows_nt') && !\$\$Registry{'LMachine\\\\Network\\\\Logon\\\\\\\\UserProfiles'}
        ? \$homewrk : \$homeusr");
 if    ($ENV{HOME} && lc($ENV{HOME}) eq lc($homeusr)) {}
 elsif (!($ENV{HOME} =$homeusr))  {}
 elsif ($os =~/Windows_NT/i)      {Run('setx','HOME',$homeusr)} # eval("use Win32::TieRegistry;\$\$Registry{'CUser\\\\Environment\\\\\\\\HOME'}=\$homeusr")}
 elsif ($ENV{windir})             {Run('winset',"HOME=$homeusr")}

 1;
},0}
