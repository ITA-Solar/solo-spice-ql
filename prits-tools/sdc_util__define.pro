;+
; Project     : Hinode Science Data Centre Europe (Oslo SDC)
;                   
; Name        : SDC_UTIL__DEFINE
;               
; Purpose     : ::get(), ::set, ::tags(), ::classdef(), ::debug, ::private
;               
; Explanation : This is a "generic, useful toy & teaching example" for object
;               oriented programming. Adds generic methods to get and set tag
;               values inside an object, to return the list of tags, and to
;               return the class definition hierarchy. It also makes it
;               extremely easy to create read- & write-sensitive tags.
;               Finally, implements a method to safeguard private methods
;               (those that should only be called by other methods of the same
;               object) and strictly private methods (those that should only
;               be called by methods in the same class).
;
; Use         : Inherit the class into your class definition, and you have the
;               following methods intended for "general use":
;               
;               o->get(/TAG_NAME)
;
;                  Returns the value of the tag indicated. Unique
;                  abbreviations are allowed.
;
;               o->set,tag_name=value [,tag2_name=value2 ...]
;
;                  Sets the values of the indicatedtags. Unique abbreviations
;                  allowed.
;
;                  Read- & write-sensitive tags:
;
;                  If you want something special to happen whenever someone
;                  modifies a particular tag, create a method by the name of
;                  "set_<tag_name>". This will automatically be called
;                  whenever someone does "o->set,tag_name=value". The value is
;                  sent as the only positional parameter.
;
;                  If you want something special to happen whenever someone
;                  *reads* a particular tag, create a method function called
;                  "get_<tag_name>()". This will automatically be called
;                  whenever someone calls "o->get(/tag_name)".
;
;                  Read & write sensitivity can of course be used to create
;                  "read-only", "pseudo" or even "hidden" tags - and it's
;                  completely up to you to decide what to do if a user tries
;                  to modify a read-only tag or read a hidden tag. See the
;                  implementation of ::set_sdc_util_version and
;                  ::get_sdc_util_version() below.
;
;                  No "/VALUE", "/NO_COPY", or "/ALL" keyword:
;                  
;                  If you want the value pointed to by a pointer - get the
;                  pointer, dereference it, and forget about no-copy. And if
;                  you think you need to to access all the tags of an object
;                  in one go - write your routine[s] as a method of the class
;                  you're working with (or create a subclass)!
;                  
;               o->tags()
;
;                  Returns a string array of tag names inside the object.
;                  
;               o->classdef()
;
;                  Returns a complete description of the class definition in
;                  terms of the class name and (recursively), its superclasses.
;                  
;               o->debug
; 
;                  Sets o=self and stops...
;
;               o->private [,/strictly]
;
;                  Invoke at the beginning of "private" routines - those that
;                  are only meant for use by other methods of the same object.
;                  To also prevent super- or subclasses from using the method,
;                  set the /strictly flag.
;                  
; Inputs      : None.
; 
; Opt. Inputs : None.
;
; Outputs     : See Use above
;               
; Opt. Outputs: 
;               
; Keywords    : ::INIT(/QUIET) disables printing of ::classdef()
;
; Calls       : 
;
; Common      : None
;               
; Restrictions: 
;               
; Side effects: 
;               
; Categories  : Object utility
;               
; Prev. Hist. : None
;
; Written     : SVH Haugan, UiO, 25 September 2007
;               
; Modified    : Relevant log entries below:
;               $Log: not supported by cvs2svn $
;               Revision 1.9  2007/12/06 09:18:51  tfredvik
;               # set: remove help,extra,/str
;
;               Revision 1.8  2007/12/05 14:29:51  steinhh
;               Fixed problem with help,calls=calls output: different in virtual machine mode
;
;               Revision 1.7  2007/10/08 08:16:37  steinhh
;               Added documentation for ::private
;
;               Revision 1.4  2007/10/08 07:53:45  steinhh
;               Added ::private
;
;               Revision 1.3  2007/10/05 13:24:57  steinhh
;               Added documentation & read/write sensitivity
;
;
; Version     : $Revision: 1.10 $$Date: 2007-12-06 09:28:56 $
;-


FUNCTION sdc_util::init,quiet=quiet
  o = self                      ; Shorthand
  class = obj_class(o)
  stc = create_struct(name=class)
  tags = tag_names(stc)
  o.sdc_util_tags = ptr_new(tags,/no_copy)
  IF NOT keyword_set(quiet) THEN print,"I am a "+o->classdef()

  return,1
END


PRO sdc_util::cleanup
  ptr_free,self.sdc_util_tags  
END


FUNCTION sdc_util::classdef,name
  IF n_params() EQ 0 THEN name = obj_class(self,count=count)
  super = obj_class(name,/super,count=nsuper)
  out = name
  FOR i=nsuper-1,0,-1 DO BEGIN
     out = '('+self->classdef(super[i])+'):'+out
  END 
  return,out
END


PRO sdc_util::set,_extra=extra
  o = self
  myt = *o.sdc_util_tags
  t = tag_names(extra)
  FOR i=0,n_elements(t)-1 DO BEGIN
     ix = where(strmid(myt,0,strlen(t[i])) EQ t[i],c)
     IF c GT 1 THEN message,"Several properties match "+t[i]+": "+ $
                            strjoin(myt[ix],', ')
     IF ~c THEN message,"No property matches "+t[i]
     IF t[i] NE myt[ix[0]] THEN print,"Setting self."+myt[ix[0]]+" from extra."+t[i]
     IF NOT obj_hasmethod(o,"set_"+myt[ix[0]]) THEN o.(ix[0]) = extra.(i) $
     ELSE call_method,"set_"+myt[ix[0]],o,extra.(i)
  END 
END 

FUNCTION sdc_util::get,_extra=extra
  o = self
  myt = *o.sdc_util_tags
  t = tag_names(extra)
  IF n_elements(t) GT 1 THEN message,"Can only return 1 property at a time"
  ix = where(strmid(myt,0,strlen(t[0])) EQ t[0],c)
  IF c GT 1 THEN message,"Several properties match: "+ strjoin(myt[ix],', ')
  IF ~c THEN message,"Cannot find property "+t[0]
  IF t[0] NE myt[ix[0]] THEN print,"Returning "+myt[ix[0]]+" ("+t[0]+")"
  IF NOT obj_hasmethod(o,"get_"+myt[ix[0]]) THEN return,o.(ix[0])
  return,call_method("get_"+myt[ix[0]],o)
END

FUNCTION sdc_util::clone, object=object
  ;; Shamelessly copied from http://www.idlcoyote.com/tips/copy_objects.html .
  IF( NOT Keyword_Set(object) ) THEN object = self
  obj = object
  filename = '/tmp/clone.sav'
  save, obj, filename=filename
  obj = 0                      
  restore, filename
  file_delete, filename, /quiet
  return, obj
END

PRO sdc_util::set_sdc_util_version,val
  on_error,1
  message,"How dare you try to change sdc_util_version!"
END


FUNCTION sdc_util::get_sdc_util_version
  ;; Return to main level, where the value is not available!
  return,trim(double(strmid('$Revision: 1.10 $',11,100)))
END 


FUNCTION sdc_util::tags
  return,*self.sdc_util_tags
END 


PRO sdc_util::private,strictly=strictly
  on_error,2
  strictt = keyword_set(strictly) ? "strictly " : ""
  help,calls=calls ; this produces no output!
  IF strpos(calls[1],' ') GT 0 THEN BEGIN 
     usr = strmid(calls[2],0,strpos(calls[2],' '))
     cli = strmid(calls[1],0,strpos(calls[1],' '))
  END ELSE BEGIN
     usr = calls[2]
     cli = calls[1]
  END
  msg = cli + ' is a '+strictt+'private method'
  IF NOT keyword_set(strictly) THEN BEGIN
     meth = strmid(usr,strpos(usr,'::')+2,1000)
     IF obj_hasmethod(self,meth) THEN return
     message,msg
  END
  
  usrclass = strmid(usr,0,strpos(usr,'::'))
  cliclass = strmid(cli,0,strpos(cli,'::'))
  IF usrclass EQ cliclass THEN return
  message,msg
END

PRO sdc_util::debug
  o = self
  stop
END 


PRO sdc_util__define
  pseudo = 0b
  ptr = ptr_new()
  d = { sdc_util, $
        sdc_util_version:pseudo,$
        sdc_util_tags:ptr $
      }
END
