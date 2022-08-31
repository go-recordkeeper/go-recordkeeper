from pathlib import Path

from setuptools import find_namespace_packages, setup

readme_file = Path(__file__).parent / 'README.md'
if readme_file.exists():
    with readme_file.open() as f:
        long_description = f.read()
else:
    # When this is first installed in development Docker, README.md is not available
    long_description = ''

setup(
    name='goban-python',
    description='',
    long_description=long_description,
    long_description_content_type='text/markdown',
    license='MIT',
    author='Daniel Chiquito',
    author_email='daniel.chiquito@gmail.com',
    keywords=['go', 'baduk', 'weiqi', 'django'],
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Environment :: Web Environment',
        'Framework :: Django :: 4.1',
        'Framework :: Django',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python',
    ],
    python_requires='>=3.10',
    packages=find_namespace_packages(include=['goban*']),
    include_package_data=True,
    install_requires=[
        'django~=4.1.0',
        'django-extensions',
        'djangorestframework',
        'psycopg2',
    ],
    extras_require = {
        'test': [
            'pytest',
            'pytest-django',
            'tox',
        ]
    }
)
