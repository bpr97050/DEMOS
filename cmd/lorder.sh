
#
#       Определение взаимосвязи об'ектных файлов.
#
#       lorder файл ...
#
#       Стандартное местоположение в системе:  /bin
#
#       $Header$
#       $Log$
#
trap "rm -f $$sym?ef; exit" 0 1 2 13 15
case $# in
0)      echo usage: lorder file ...
        exit ;;
1)      case $1 in
        *.o)    set $1 $1
        esac
esac
nm -g $* | sed '
        /^$/d
        /:$/{
                /\.o:/!d
                s/://
                h
                s/.*/& &/
                p
                d
        }
        /[TD] /{
                s/.* //
                G
                s/\n/ /
                w '$$symdef'
                d
        }
        s/.* //
        G
        s/\n/ /
        w '$$symref'
        d
'
sort $$symdef -o $$symdef
sort $$symref -o $$symref
join $$symref $$symdef | sed 's/[^ ]* *//'
