{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
EXIF information
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}
{% if id.exif %}
<table>
    {% for row in id.exif %}
    <tr>
        <th title="{{ row.tag|escape }}">{{ row.name|escape }}</th>
        <td>{{ row.value|escape }}</td>
    </tr>
    {% endfor %}
</table>
{% endif %}

{% endblock %}
