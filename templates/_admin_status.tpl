{% if m.acl.use.mod_exif %}
<div class="form-group">
    <div>
    {% button class="btn btn-default" text=_"Rescan EXIF information" postback={rescan_exif} delegate="mod_exif" %}
    <span class="help-block">{_ Rescanning EXIF information will get all EXIF info from all images and store it. _}</span>
</div>
{% endif %}
