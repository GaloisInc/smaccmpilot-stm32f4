==
[Logicech F710 Joystick](http://logitech-en-amr.custhelp.com/app/answers/detail/a_id/21426/section/troubleshoot/crid/411/lt_product_id/7361/tabs/1,3,2,4,5/cl/us,en)

Leave in 'D' configuration.

==

--------------------------------------------------------------------------------

== [Joysticks on Fedora](https://www.blakerohde.com/blog/2012/06/gamepads-joysticks-on-fedora-17/) ==

    > sudo yum install kernel-modules-extra
    > sudo yum install joystick

Now test: (http://pingus.seul.org/~grumbel/jstest-gtk/):

    > jstest --event /dev/input/js0

You may wish to test/calibrate with
[jstest-gtk](http://pingus.seul.org/~grumbel/jstest-gtk/)

--------------------------------------------------------------------------------

== [Joysticks with MAVProxy](http://diydrones.com/profiles/blogs/sitl-mavproxy-joystick-support) ==

[Pygame](pygame.org/install.html)

I installed via

    > sudo yum install pygame

from mavproxy:

    > module load mavproxy_joystick

should report

    joystick found: Logitech Logitech Cordless RumblePad 2

--------------------------------------------------------------------------------
