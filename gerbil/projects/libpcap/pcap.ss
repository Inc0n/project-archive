
(import :std/foreign)

(def +error-buffer-size+ 256) ; PCAP_ERRBUF_SIZE
(def +PCAP_IF_LOOPBACK+  1)


;; Supported datalink types
;; TODO: Add more from pcap-bpf.h
;;       Maybe use cffi-grovel? Needs headers to be present at compilation.

(def +DLT_NULL+          0)
(def +DLT_EN10MB+        1)
(def +DLT_SLIP+          8)
(def +DLT_PPP+           9)
(def +DLT_PPP_BSDOS1+   14)
(def +DLT_PPP_BSDOS2+   16)
(def +DLT_PPP_SERIAL+   50)
(def +DLT_PPP_ETHER+    51)
(def +DLT_PPP_PPPD+    166)
(def +DLT_802_11+      105)
(def +DLT_USB_LINUX+   189)

(def *supported-datalinks*
  `(("NULL"         . ,+DLT_NULL+)
    ("EN10MB"       . ,+DLT_EN10MB+)
    ("SLIP"         . ,+DLT_SLIP+)
    ("PPP"          . ,+DLT_PPP+)
    ("PPP-BSDOS"    . ,+DLT_PPP_BSDOS1+)
    ("PPP-BSDOS"    . ,+DLT_PPP_BSDOS2+)
    ("PPP-SERIAL"   . ,+DLT_PPP_SERIAL+)
    ("PPP-ETHER"    . ,+DLT_PPP_ETHER+)
    ("PPP-PPPD"     . ,+DLT_PPP_PPPD+)
    ("802.11-WLAN"  . ,+DLT_802_11+)
    ("USB-LINUX"    . ,+DLT_USB_LINUX+)))

(def *pcap-version* nil "Version of native libpcap library.")

(begin-ffi ((struct timeval tv-sec tv-usec)
            (struct pcap-pkthdr ts caplen len)
            (struct pcap-if-t next name description address flags)
            (struct pcap-addr-t next addr netmask broadaddr dstaddr))
  (c-declare "#include <pcap.h>")
  ;; timeval structure.
  (define-c-struct timeval              ; Force 32bit sec/usec
    ((tv-sec long)
     (tv-usec long)))
  ;; Packet header structure
  (define-c-struct pcap-pkthdr
    ((ts . timeval)
     (caplen . unsigned-int32)
     (len . unsigned-int32)))
  (define-c-struct pcap-if-t
    ((next . pcap-if-t*)
     (name . char-string)
     (description . char-string)
     (address . pcap-addr-t*)
     (flags . unsigned-int32)))
  (define-c-struct pcap-addr-t
    ((next . pcap-addr-t*)
     (addr . sockaddr*)
     (netmask . sockaddr*)
     (broadaddr . sockaddr*)
     (dstaddr . sockaddr*)))
  ;; (define-c-lambda %pcap-open-live (char-string int int int char-string) pcap-if-t
  ;;   "__return(%pcap-open-live(__arg1, __arg2, __arg3, __arg4, __arg5))")
  (define-c-lambda %pcap-open-live (char-string int int int char-string) pcap-t*
    "__return(pcap_open_live(__arg1, __arg2, __arg3, __arg4, __arg5))")
  (define-c-lambda %pcap-open-dead (int int) pcap-t*
    "__return(pcap_open_dead(__arg1, __arg2))")
  (define-c-lambda %pcap-open-offline (char-string char-string) pcap-t*
    "__return(pcap_open_offline(__arg1, __arg2))")
  ;;
  (define-c-lambda %pcap-findalldevs (pcap-if-t* char-string) int
    "__return(pcap_findalldevs(__arg1, __arg2))")
  (define-c-lambda %pcap-freealldevs (pcap-if-t*) void
    "pcap_freealldevs(__arg1)")
  ;; deprecated
  (define-c-lambda %pcap-lookupdev (char-string) char-string
    "__return(pcap_lookupdev(__arg1))")
  (define-c-lambda %pcap-loop (pcap-t*
                          int
                          (nonnull-function (unsigned-char* pcap-pkthdr* unsigned-char*) void)
                          char-string)
    void
    "pcap_loop(__arg1, __arg2, __arg3, __arg4)")
  (define-c-lambda %pcap-dump
    (unsigned-char* pcap_pkthdr* unsigned-char*) void
    "pcap_dump(__arg1, __arg2,  __arg3)")
  (define-c-lambda %pcap-compile
    (pcap-t* bpf-program* char-string int unsigned-int32) int
    "__return(pcap_compile(__arg1, __arg2,  __arg3, __arg4, __arg5))"))
